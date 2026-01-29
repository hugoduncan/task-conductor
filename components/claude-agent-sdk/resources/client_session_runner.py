"""Helper classes for running Claude SDK client operations.

ClientSessionRunner runs client operations in a single persistent async Task,
ensuring all async operations run in the same task context as required by
anyio's cancel scope tracking.
"""

import asyncio
import concurrent.futures
import queue
import threading


class ClientSessionRunner:
    """Runs client operations in a single persistent async Task.

    This ensures all async operations run in the same task context,
    which is required by anyio's cancel scope tracking. Commands are
    sent via a thread-safe queue and results returned via futures.

    Optionally accepts an event_callback function that is called for
    each message received during query operations, enabling real-time
    event capture for monitoring and debugging.
    """

    def __init__(self, client, event_callback=None):
        self.client = client
        self.event_callback = event_callback
        self.loop = asyncio.new_event_loop()
        self._command_queue = queue.Queue()
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._running = True
        self._thread.start()

    def _run_loop(self):
        """Run the event loop in the background thread."""
        asyncio.set_event_loop(self.loop)
        self.loop.run_until_complete(self._process_commands())

    async def _process_commands(self):
        """Process commands from the queue in a single async task."""
        while self._running:
            try:
                # Check for commands with a timeout to allow shutdown
                cmd = await asyncio.get_event_loop().run_in_executor(
                    None, lambda: self._command_queue.get(timeout=0.1)
                )
            except queue.Empty:
                continue

            cmd_type, args, result_future = cmd

            try:
                if cmd_type == "connect":
                    prompt = args.get("prompt")
                    if prompt:
                        await self.client.connect(prompt)
                    else:
                        await self.client.connect()
                    result_future.set_result(None)

                elif cmd_type == "query":
                    prompt = args["prompt"]
                    await self.client.query(prompt)
                    messages = []
                    async for msg in self.client.receive_response():
                        messages.append(msg)
                        if self.event_callback:
                            try:
                                self.event_callback(msg)
                            except Exception:
                                # Don't let callback errors break the flow
                                pass
                    result_future.set_result(messages)

                elif cmd_type == "disconnect":
                    await self.client.disconnect()
                    self._running = False
                    result_future.set_result(None)

                elif cmd_type == "shutdown":
                    self._running = False
                    result_future.set_result(None)

            except Exception as e:
                result_future.set_exception(e)

    def _send_command(self, cmd_type, args=None, timeout=None):
        """Send a command and wait for result.

        Args:
            cmd_type: The command type string
            args: Optional dict of command arguments
            timeout: Optional timeout in seconds. None means wait indefinitely.

        Raises:
            TimeoutError: If timeout expires before command completes
        """
        result_holder = concurrent.futures.Future()
        self._command_queue.put((cmd_type, args or {}, result_holder))
        try:
            return result_holder.result(timeout=timeout)
        except concurrent.futures.TimeoutError:
            raise TimeoutError(f"{cmd_type} operation timed out after {timeout}s")

    def connect(self, prompt=None, timeout=None):
        """Connect the client.

        Args:
            prompt: Optional initial prompt string
            timeout: Optional timeout in seconds
        """
        return self._send_command("connect", {"prompt": prompt}, timeout=timeout)

    def query(self, prompt, timeout=None):
        """Send a query and receive response.

        Args:
            prompt: The prompt to send
            timeout: Optional timeout in seconds
        """
        return self._send_command("query", {"prompt": prompt}, timeout=timeout)

    def disconnect(self, timeout=None):
        """Disconnect the client.

        Args:
            timeout: Optional timeout in seconds
        """
        return self._send_command("disconnect", timeout=timeout)

    def close(self):
        """Shutdown the runner without disconnecting."""
        self._send_command("shutdown", timeout=5.0)
        self._thread.join(timeout=5.0)


async def _collect_async_iter(aiter):
    """Collect all items from an async iterator into a list."""
    result = []
    async for item in aiter:
        result.append(item)
    return result


async def _query_and_receive(client, prompt):
    """Send a query and collect all response messages."""
    await client.query(prompt)
    result = []
    async for msg in client.receive_response():
        result.append(msg)
    return result
