# Progress

## Completed
- Added AST-level provenance handling in `LFGxDRT`.
- Added optional `[sourceIndex]` parsing to `DrsParser`.
- Ensured provenance is carried through DRS, referents, conditions, merges, and functional application.
- Added `SRC` output on discourse referents as a graph attribute-style constraint.
- Switched graph-visible provenance tokens to `iN` so they stay string-valued instead of becoming node ids.

## GSWB
- Kept the prover generic while adding a lazy LFGxDRT bridge for `Settings.LFGXDRT`.
- Normalized premises into LFGxDRT only when first encountered during `calculateSolutions()`.
- Removed the controller-side re-parse for LFGxDRT mode.
- Preserved internal integer source indices for `insitu` handling.

## LiGER
- Changed `SYN-ID` serialization to `iN` so it behaves like an attribute value.
- Updated LiGER tests to expect the string form.

## Verification
- `mvn test` in `LFGxDRT`
- `./mvnw -Dtest=LigerControllerTest,LinguisticStructureMergerTest test` in `liger`
- `mvn test` in `GlueSemWorkbench_v2`

## Notes
- The shared graph now keeps provenance as string-valued attributes rather than integer-valued nodes.
- GSWB still stores the source index internally as an integer for `insitu` logic.
