# === Basic **kwargs unpacking ===
def greet(name, greeting):
    return f'{greeting}, {name}!'


opts = {'greeting': 'Hi'}
assert greet('Alice', **opts) == 'Hi, Alice!', 'basic **kwargs unpacking'

# === Dict literal unpacking ===
assert greet('Charlie', **{'greeting': 'Hey'}) == 'Hey, Charlie!', 'dict literal unpacking'


# === Multiple kwargs in unpacked dict ===
def format_msg(msg, prefix, suffix):
    return f'{prefix}{msg}{suffix}'


assert format_msg('test', **{'prefix': '[', 'suffix': ']'}) == '[test]', 'multiple kwargs unpacking'

# === Combining regular kwargs with **kwargs ===
assert format_msg('hello', prefix='> ', **{'suffix': '!'}) == '> hello!', 'regular kwargs with **kwargs'


# === **kwargs with positional args ===
def add_all(a, b, c):
    return a + b + c


assert add_all(1, 2, **{'c': 3}) == 6, '**kwargs with positional args'
assert add_all(1, **{'b': 2, 'c': 3}) == 6, '**kwargs providing multiple args'

# === Variable dict unpacking ===
settings = {'prefix': '>>> ', 'suffix': ' <<<'}
assert format_msg('output', **settings) == '>>> output <<<', 'variable dict unpacking'


# === Unpacking with keyword-only args ===
def kwonly_func(a, *, b, c):
    return a + b + c


assert kwonly_func(1, **{'b': 2, 'c': 3}) == 6, '**kwargs with keyword-only args'


# === Empty dict unpacking with all args provided ===
def simple(x, y):
    return x + y


assert simple(1, 2, **{}) == 3, 'empty dict unpacking'


# === All kwargs from unpacking ===
def all_kwargs(a, b, c):
    return a * 100 + b * 10 + c


assert all_kwargs(**{'a': 1, 'b': 2, 'c': 3}) == 123, 'all args from **kwargs'
assert all_kwargs(**{'c': 7, 'a': 4, 'b': 5}) == 457, 'all args from **kwargs different order'


# === Dynamic **kwargs keys ===
def kwonly_echo(*, keyword):
    return keyword


key_name = 'k' + 'e' + 'y' + 'w' + 'o' + 'r' + 'd'
assert kwonly_echo(**{key_name: 'dynamic'}) == 'dynamic', 'runtime string key matches kw-only param'
