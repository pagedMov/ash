# Rsh

Rsh is a modern shell designed to combine the power of traditional shell scripting with the simplicity and extensibility of modern scripting languages. Rsh aims to address the pain points of existing shells like Bash while remaining familiar and backward-compatible.

Development for this project is still in it's infancy, and currently is far from being usable.

## Vision

1. **Improved Developer Experience**
   - Detailed error messages with line/column tracking.
   - Support for both bash's weakly typed variables and variables that are strongly typed using new typing builtins:
```bash
# Bash syntax, weakly typed
var="hello"
var=1

# Strongly typed using type builtins
int var=1
string var="hello"
```
---

2. **Macros**
   - A middleground between aliases and functions. Useful for very common operations that are too complex for aliases but too simple to justify defining an entire function.
   - Macros are expanded at runtime like aliases, but avoid creating an entirely new scope like functions.

A macro is defined like:
```bash
macro split { "$cmd" | tr ' ' '\n' }
```
And is called like:
```bash
split!echo "hello world"
```
Which expands into:
```bash
echo "hello world" | tr ' ' '\n'
```
And evaluates to:
```bash
"hello"
"world"
```
---

3. **Multi-Interpreter Support**
   - Dynamically offload tasks to secondary interpreters in a subshell by declaring a shebang within the subshell.
   - Utilize the system-level control of the shell and the programmatic power of scripting languages like Python or Perl all in one file.
```bash
#!/usr/bin/env rsh

echo "Greetings from rsh!"

(
  #!/usr/bin/env python3
  print("Greetings from python!")
)

output=$(
  #!/usr/bin/env python3
  print("This output is stored in a variable")
)
```
---

4. **Backward Compatibility**
   - Fully compatible with existing Bash scripts, while providing additional tools for modernization.
---

5. **Portability and Consistency**
   - Unified syntax and new builtins which cover the most common shell functionalities used in scripts minimize dependency on external commands and ensure rsh scripts work consistently across environments.

Rsh is an evolving project focused on bridging the gap between traditional shell scripting and modern scripting language paradigms.

## Why Rsh?
Bash is undoubtedly an extremely powerful tool, but really shows its age with limited error handling, clunky data structures, and arcane syntax. Rsh aims to modernize shell scripting by incorporating:
- Familiar and intuitive syntax.
- Features inspired by modern languages like Python and Rust.
- Compatibility with existing Bash scripts.

## Contributing
Contributions are more than welcome. Feel free to open isses or submit pull requests.

## License
This project is licensed under the MIT License.
