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
    """

    def __init__(self, client):
        self.client = client
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

    def _send_command(self, cmd_type, args=None):
        """Send a command and wait for result."""
        result_holder = concurrent.futures.Future()
        self._command_queue.put((cmd_type, args or {}, result_holder))
        return result_holder.result()

    def connect(self, prompt=None):
        """Connect the client."""
        return self._send_command("connect", {"prompt": prompt})

    def query(self, prompt):
        """Send a query and receive response."""
        return self._send_command("query", {"prompt": prompt})

    def disconnect(self):
        """Disconnect the client."""
        return self._send_command("disconnect")

    def close(self):
        """Shutdown the runner without disconnecting."""
        self._send_command("shutdown")
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
