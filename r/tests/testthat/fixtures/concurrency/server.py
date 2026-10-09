"""Deterministic threaded provider fixture; no external service or model."""
import json
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

lock = threading.Lock()
active = 0
maximum = 0
calls = []
attempts = {}


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def send(self, body, status=200, retry=None):
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(data)))
        if retry is not None:
            self.send_header('Retry-After', str(retry))
        self.end_headers()
        try:
            self.wfile.write(data)
        except (BrokenPipeError, ConnectionResetError):
            pass

    def do_GET(self):
        with lock:
            data = dict(maximum=maximum, active=active, calls=list(calls))
        self.send(data)

    def do_POST(self):
        global active, maximum
        body = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        messages = body.get('messages', [])
        prompt = body.get('state') or next((m['content'] for m in messages if m['role'] == 'user'), '')
        if isinstance(prompt, list):
            prompt = next(p.get('text', '') for p in prompt if p.get('type') == 'text')
        with lock:
            active += 1
            maximum = max(maximum, active)
            attempts[prompt] = attempts.get(prompt, 0) + 1
            attempt = attempts[prompt]
            record = dict(prompt=prompt, body=body, started=time.time(), attempt=attempt)
            calls.append(record)
        try:
            time.sleep(0.4 if 'slow' in prompt else 0.12)
            if 'fail' in prompt:
                return self.send({'error': {'message': 'fixture failure'}}, 400)
            if 'retry' in prompt and 'tool_retry' not in prompt and attempt == 1:
                return self.send({'error': {'message': 'retry fixture'}}, 429, 0.15)
            if 'always_busy' in prompt:
                return self.send({'error': {'message': 'busy fixture'}}, 503, 0)
            if 'long_cooldown' in prompt:
                return self.send({'error': {'message': 'wait fixture'}}, 429, 120)
            if self.path.endswith('/systemone'):
                answers = {}
                for key, question in body['questions'].items():
                    if question['type'] == 'noul':
                        answers[key] = dict(type='noul', noul=0.8)
                    else:
                        options = list(question['criteria'])
                        probabilities = {k: (1.0 if i == 0 else 0.0) for i, k in enumerate(options)}
                        answers[key] = dict(type='choice', choice=options[0], probabilities=probabilities, confidence=1)
                return self.send(dict(model='fixture', answers=answers, usage=dict(input_tokens=10, output_tokens=0)))
            message = dict(role='assistant', content=prompt)
            if body.get('tools'):
                tool_results = [m for m in messages if m['role'] == 'tool']
                if not tool_results:
                    message = dict(role='assistant', content=None, reasoning_content='fixture reasoning', tool_calls=[dict(
                        id='call_' + prompt, type='function', function=dict(name='unauthorized' if 'unauthorized' in prompt else 'add_numbers', arguments='{"x":2,"y":3}')
                    )])
                else:
                    if 'tool_retry' in prompt and attempt == 2:
                        return self.send({'error': {'message': 'tool followup retry'}}, 429, 0.1)
                    message['content'] = prompt + ': ' + str(tool_results[-1]['content'])
            if body.get('response_format'):
                message['content'] = 'invalid' if 'invalid' in prompt else '{"answer":"ok"}'
            if self.path.endswith('/api/chat'):
                return self.send(dict(model='fixture', message=message, done=True, prompt_eval_count=10, eval_count=2))
            if self.path.endswith('/messages'):
                return self.send(dict(model='fixture', type='message', role='assistant',
                                      content=[dict(type='text', text=prompt)], stop_reason='end_turn',
                                      usage=dict(input_tokens=10, output_tokens=2)))
            self.send(dict(model='fixture', choices=[dict(message=message, finish_reason='stop')],
                           usage=dict(prompt_tokens=10, completion_tokens=2)))
        finally:
            with lock:
                record['finished'] = time.time()
                active -= 1


server = ThreadingHTTPServer(('127.0.0.1', 0), Handler)
with open(sys.argv[1], 'w') as f:
    f.write(str(server.server_port))
server.serve_forever()
