# todo

## Refactor `Command.ml`
- [ ] `check_updated_ctx`
- [ ] `check_valid_constructor`
- [ ] `build_lts_graph`
- [ ] `build_graph`
- [ ] `lts_graph_to_lts`

## Add Plugin Support Structural Equivalence
If provided, when building the LTS of a coq-term the plugin should only consider exploring new states that are not structurally equivalent to an already-explored state.


## Overaul `Utils.params`
Overhaul the `params` used to determine the level-of-detail for logging and formatting. The current approach ended up being rather combersome and, unfortunately difficult to extend and use.
- Function `Utils.log` handles all printing and ensures the correct output is used (i.e., if in Coq then using `Feedback`).
- However, there is currently some overlap with the idea of "show details": some messages are classified as a "detail", whilst others will provide "further details" (e.g., `Fsm.PStr` will include additional meta-info).

