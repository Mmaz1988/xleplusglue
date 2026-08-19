# Analysis sequence append redundancy

## Bug 1: `Add sentence` reparses previously accepted sentences

### Summary

In the analysis UI, `LigerVis.addSentence()` tries to reuse already parsed
sentence structures when appending a new sentence, but the request shape does
not match LiGER's `/apply_rules_xle_sequence` contract.

The frontend sends `sentences = old sentences + new sentence`, while
`parsedSentences` contains only the previously cached old sentence structures.
LiGER only honors `parsedSentences` when every sentence has at least one
supplied structure, so the partial payload is ignored and the endpoint falls
back to reparsing every sentence in the sequence.

### Observed flow

1. The first sentence is parsed through `LigerVis.analyzeSentence()`.
2. The resulting sequence/sentence structures are cached in
   `parsedSentenceStructures`.
3. When the user clicks `Add sentence`, `LigerVis.addSentence()` builds:
   - `sentences`: all previous sentences plus the new sentence
   - `parsedSentences`: only the previous sentence structures
4. LiGER checks whether `parsedSentences.size() == sentences.size()`.
5. The check fails, so LiGER calls XLE again for the sequence text rather than
   reusing the old sentence structures.

### Why this is a problem

- Old sentences are reparsed even though their accepted structures are already
  known.
- The frontend comment says the old structures are reused so only the new
  sentence is parsed, but that is not what the backend does for this payload.
- The backend DTO has `parsedLastSentence`, but the client does not send it, and
  that field would reuse the last sentence rather than express "reuse the old
  sentences and parse the new one."

### Relevant files

- `../xleplusglue-client/src/app/liger-vis/liger-vis.component.ts`
- `../liger/src/main/java/de/ukon/liger/webservice/rest/LigerController.java`
- `../liger/src/main/java/de/ukon/liger/webservice/rest/dtos/LigerSequenceRequest.java`

### Expected behavior

Appending a sentence should avoid reparsing already accepted old sentences. The
frontend/backend contract should support an append shape such as:

- old sentence or sequence structures are supplied,
- the new sentence text is parsed once,
- the merged sequence is assembled from those parts.

## Bug 2: Analysis performs a second sequencing call after GSWB semantics

### Summary

After `Add sentence` returns from LiGER, the analysis flow later calls
`/apply_rules_xle_sequence` again from `GswbVis.mergeSyntaxForResults()`.

The first call already produced a merged sequence syntax and cached its parsed
structures. The second call reconstructs sequence syntax from those structures
so the GSWB-merged semantic result can receive a canonical `SequenceAnalysis`.
That means the analysis append path does sequence construction twice for one
discourse extension.

### Observed flow

1. `LigerVis.addSentence()` calls `/apply_rules_xle_sequence`.
2. The response is cached, and proof inputs are emitted for the newly appended
   sentence's sequence part.
3. The user runs GSWB semantics for that new sentence part.
4. `GswbVis.mergeCurrentSolutions()` calls GSWB `/merge_sequence_semantics` to
   merge the prior semantic context with the new sentence semantics.
5. `GswbVis.mergeSyntaxForResults()` calls LiGER `/apply_rules_xle_sequence`
   again, this time with parsed structures for all sentences, to attach merged
   sequence syntax to the semantic result.

### Why this is a problem

- The second sequencing call repeats sequence assembly work that the first call
  already performed.
- The workflow moves sequence syntax from LiGER to GSWB and then asks LiGER to
  reconstruct it again, instead of carrying the first merged syntax forward.
- The duplication makes the analysis path harder to reason about and may hide
  mismatches between the syntax used for proof inputs and the syntax attached
  to the final `SequenceAnalysis`.

### Relevant files

- `../xleplusglue-client/src/app/liger-vis/liger-vis.component.ts`
- `../xleplusglue-client/src/app/gswb-vis/gswb-vis.component.ts`

### Expected behavior

The analysis append flow should assemble the sequence once and carry that
resulting syntax forward into the GSWB merge/document model. If a second phase
needs to attach semantic results to a `SequenceAnalysis`, it should reuse the
sequence syntax returned by the append call rather than calling LiGER sequencing
again.

