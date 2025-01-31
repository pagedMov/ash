## Stuff that needs to be done

* Rework command substitution; it's really fucking slow. It shouldn't use ox-based subshells, it should just execute the commands in the current shell. If a subshell is necessary for whatever the user is doing in the command sub, they can just use $((commands here)) to insert the subshell into the command sub
