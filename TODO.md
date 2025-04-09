# todo

## Layered Semantics Handling in `command.ml`
Currently is rather "hacky" -- tries each coq-lts in a list, one by one until the correct constructor is found in one of them.
-[x] Change this to use a map since we should already have access to the type of the term anyway.


## Overaul `Utils.params`
Overhaul the `params` used to determine the level-of-detail for logging and formatting. The current approach ended up being rather combersome and, unfortunately difficult to extend and use.
- Function `Utils.log` handles all printing and ensures the correct output is used (i.e., if in Coq then using `Feedback`).
- However, there is currently some overlap with the idea of "show details": some messages are classified as a "detail", whilst others will provide "further details" (e.g., `Fsm.PStr` will include additional meta-info).

