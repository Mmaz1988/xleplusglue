# Neuro-symbolic Coreference Plan

## Objective

Develop a staged neuro-symbolic approach to coreference resolution in which:

- formal syntax and compositional semantics generate structurally admissible antecedent mappings;
- provenance links connect discourse referents and semantic conditions to their source-token spans;
- contextual embeddings rank or filter the formally admissible mappings;
- resolved DRSs are passed to the existing NLI and theorem-proving pipeline;
- NLI items provide task-based evaluation and, later, weak supervision for coreference decisions.

The initial aim is not to build a complete neural coreference resolver. The formal system remains responsible for mention identification, accessibility, binding constraints, semantic construction, and inference. The neural component supplies graded lexical and contextual compatibility.

## Core representation

Each token produced by the formal parser should have:

```text
Token {
    id: stable parser token ID
    form: surface form
    char_start: inclusive character offset
    char_end: exclusive character offset
}
```

Every relevant semantic object should retain provenance:

```text
SemanticProvenance {
    semantic_id: discourse referent, condition, or box ID
    token_ids: ordered source-token IDs
}
```

A mention span is represented using parser token IDs:

```text
Span {
    token_ids: ordered token IDs
}
```

Character offsets are the canonical bridge between parser tokens and the language model's subword tokens. Token indices alone are unsafe because the parser and XLM-R need not tokenize the text identically.

## Target embedding service

The first implementation should expose a small model-independent interface:

```text
embed_span(
    text,
    parser_tokens,
    span_token_ids,
    context = full sentence or local discourse
) -> SpanEmbedding
```

For the immediate pairwise use case:

```text
embed_pair(
    text,
    parser_tokens,
    first_span_token_ids,
    second_span_token_ids
) -> {
    first_embedding,
    second_embedding,
    cosine_similarity,
    alignment_metadata
}
```

The complete context should be encoded once. Both span embeddings must be extracted from the same contextualized model pass rather than encoding the two strings independently.

The result should record enough information for inspection and regression testing:

```text
SpanEmbedding {
    vector
    parser_token_ids
    character_ranges
    model_subword_indices
    model_subword_strings
    pooling_method
    model_name
    model_layer
}
```

## Parser-token to subword alignment

### Alignment procedure

1. Preserve the original, unmodified input string.
2. Store inclusive/exclusive character offsets for every parser token.
3. Tokenize the same input with a Hugging Face fast tokenizer and request `offset_mapping`.
4. Convert the parser span into one or more character intervals.
5. Select every non-special model subword whose character interval overlaps the parser span.
6. Pool the selected hidden states into one span embedding.
7. Return the alignment as metadata so that it can be inspected.

A model subword with interval \([s_m,e_m)\) overlaps a source interval
\([s_p,e_p)\) when:

```text
max(s_m, s_p) < min(e_m, e_p)
```

Special tokens with empty offsets must be excluded.

### Cases to test explicitly

- punctuation split differently by the two tokenizers;
- contractions such as `can't`;
- hyphenated words;
- multi-token names and definite descriptions;
- XLM-R SentencePiece markers;
- Unicode normalization and combining characters;
- scripts without whitespace word boundaries;
- parser tokens corresponding to several subwords;
- one model token overlapping more than one parser token;
- discontinuous semantic provenance, if the formal representation permits it;
- context longer than the model's maximum input length.

Offsets must be computed against exactly the string sent to the tokenizer. Text normalization must not occur independently in the parser-token and model-token paths.

## Stage 0: integration and non-neural baselines

Before evaluating embeddings:

1. Trace each pronoun and candidate discourse referent to its parser-token span.
2. Verify parser-to-subword alignment on a small multilingual test suite.
3. Generate all formally admissible antecedent mappings.
4. Produce a resolved DRS for every mapping.
5. Run the NLI checks for every resolved DRS.

Record at least two symbolic baselines:

- most recent formally accessible antecedent;
- all formally admissible mappings retained as ambiguity.

This isolates alignment and semantic-construction errors before a neural score is introduced.

## Stage 1: plain cosine similarity

Use an unmodified pretrained multilingual encoder, initially XLM-R.

For a span \(m\) whose aligned subword representations are
\(h_{i},\ldots,h_{j}\), begin with mean pooling:

\[
g(m)=\frac{1}{j-i+1}\sum_{k=i}^{j}h_k
\]

For pronoun \(p\) and candidate antecedent \(a\):

\[
s_{\mathrm{cos}}(p,a)
=
\frac{g(p)\cdot g(a)}
{\lVert g(p)\rVert\lVert g(a)\rVert}
\]

Rank only the candidates already admitted by the formal system:

\[
\hat a
=
\underset{a\in C_{\mathrm{formal}}(p)}
{\operatorname{argmax}}\;
s_{\mathrm{cos}}(p,a)
\]

Initially, do not apply a hard similarity threshold. Record the complete ranking and score distribution. Raw cosine values are not calibrated coreference probabilities.

### Initial pooling comparison

Mean pooling is the default. If it is clearly uninformative, compare it with:

- first-subword representation;
- last-subword representation;
- endpoint concatenation;
- representation from selected upper layers or an average of the last four layers.

This comparison should remain small so that it does not turn the baseline into extensive model engineering.

### Stage 1 evaluation

Report:

- top-1 antecedent accuracy;
- pairwise ranking accuracy;
- mean reciprocal rank;
- NLI accuracy after choosing the highest-ranked mapping;
- oracle NLI accuracy when any formally generated mapping may be selected;
- coverage: proportion of NLI items for which exactly one mapping yields the gold label;
- score margins between the best and second-best candidates.

The difference between oracle NLI accuracy and cosine-selected NLI accuracy estimates the contribution of antecedent ranking while controlling for formal coverage.

## NLI-derived evaluation signal

For an NLI item with gold label \(y\), let \(R\) be the formally generated set of coreference resolutions. Each resolution \(r\) produces a DRS \(D_r\). Define:

\[
R_y
=
\{r\in R :
\operatorname{NLI}(D_r,H)=y\}
\]

Classify items as:

| Result | Use |
|---|---|
| \(\lvert R_y\rvert=1\) | Informative coreference evaluation item |
| \(\lvert R_y\rvert>1\) | Coreference is underdetermined by this NLI item |
| \(\lvert R_y\rvert=0\) | Inspect parsing, axioms, gold label, or formal coverage |

Only items with a unique inference-compatible mapping provide a straightforward target for the initial ranking evaluation. Later training may use set-valued objectives for underdetermined cases.

Where possible, use controlled minimal pairs and split data by schema or reasoning pattern, not by individual sentence. Closely related members of a schema must remain in the same split.

## Stage 2: extracted gender directions

Proceed to this stage if full-vector cosine similarity provides little or no useful ranking signal.

Estimate gender directions from contextualized lexical anchor pairs such as:

```text
woman – man
girl – boy
mother – father
she – he
```

Place anchors in several matched, relatively neutral templates. For each pair:

\[
d_i=g(\text{female}_i)-g(\text{male}_i)
\]

Estimate a direction using either the normalized mean difference or the first principal component:

\[
d_{\mathrm{gender}}
=
\frac{\operatorname{mean}_i(d_i)}
{\lVert\operatorname{mean}_i(d_i)\rVert}
\]

Project a mention onto the direction:

\[
\gamma(m)=g(m)\cdot d_{\mathrm{gender}}
\]

Rank compatibility using the distance between pronoun and antecedent projections:

\[
s_{\mathrm{gender}}(p,a)
=
-\left|\gamma(p)-\gamma(a)\right|
\]

Test pronoun-derived and nominal-derived directions separately before combining them. For multilingual use, compare language-specific directions, a joint multilingual direction, and cross-lingual transfer.

Interpretation must distinguish:

- grammatical gender;
- lexically encoded natural gender;
- distributional gender associations;
- contextually inferred referential gender.

The experiment should document potential stereotypical associations, especially for names and occupational nouns.

## Stage 3: classifier over frozen contextual representations

If manually selected directions remain insufficient, freeze XLM-R and train a small pairwise classifier.

Input:

\[
[g_p;g_a;g_p\odot g_a;|g_p-g_a|]
\]

Optional symbolic or positional features can be added separately:

- sentence and mention distance;
- person, number, and gender agreement;
- grammatical functions;
- mention types;
- animacy or semantic type;
- formal accessibility.

The preferred objective is antecedent ranking over the formally generated candidate set:

\[
P(a\mid p,C)
=
\frac{\exp s(p,a)}
{\sum_{a'\in C\cup\{\epsilon\}}\exp s(p,a')}
\]

Include an unresolved antecedent \(\epsilon\) where the data and representation permit exophoric or missing antecedents.

Training negatives should come from formally admissible competing antecedents rather than arbitrary mentions. These are the alternatives the system must actually distinguish.

## Stage 4: learned projections

As an intermediate step between a hand-extracted direction and a full pairwise classifier, learn small pronoun and antecedent projections while keeping XLM-R frozen:

\[
s(p,a)=\cos(W_pg_p,W_ag_a)
\]

This tests whether the necessary information is already present in XLM-R but pronouns and nominals occupy differently structured regions.

## Stage 5: partial and full fine-tuning

If the frozen-encoder approaches plateau:

1. unfreeze only the final XLM-R layers;
2. compare with the fully frozen classifier;
3. proceed to full fine-tuning only if data volume and learning curves justify it.

Use development-set calibration before interpreting scores as thresholds or probabilities.

## Integration with ambiguity management

Neural scores should initially be treated as graded discriminants attached to formal mappings:

```text
CoreferenceMapping {
    pronoun_semantic_id
    antecedent_semantic_id
    formal_constraints_satisfied
    neural_score
    scoring_method
}
```

Prefer ranking or margin-based retention to immediate hard pruning:

\[
C'(p)
=
\{a\in C(p):
s(p,a)\geq \max_{b\in C(p)}s(p,b)-\delta\}
\]

This allows genuinely ambiguous mappings to survive into the existing discriminant interface. Hard thresholds can be added after the scores have been calibrated and their effect on formal recall has been measured.

## Implementation milestones

- [ ] Define parser token, span, and semantic-provenance data structures.
- [ ] Guarantee character offsets for every parser token.
- [ ] Implement XLM-R tokenization with offset mappings.
- [ ] Implement parser-span to subword alignment.
- [ ] Implement mean-pooled span embeddings.
- [ ] Implement the two-span embedding and cosine-similarity API.
- [ ] Add alignment inspection and regression tests.
- [ ] Connect semantic referents to source spans.
- [ ] Generate and retain all formally admissible coreference mappings.
- [ ] Run NLI separately for every resolved DRS.
- [ ] Identify uniquely informative NLI items.
- [ ] Evaluate symbolic baselines and raw cosine similarity.
- [ ] Decide from the learning signal whether to proceed to gender directions.
- [ ] Evaluate extracted gender directions if required.
- [ ] Train a classifier over frozen XLM-R representations if required.
- [ ] Evaluate learned projections and partial fine-tuning if required.
- [ ] Calibrate scores before introducing hard pruning.

## Immediate proof of concept

The first executable prototype should accept:

```text
text
parser token list with IDs and character offsets
pronoun span as parser token IDs
antecedent span as parser token IDs
```

and return:

```text
pronoun embedding
antecedent embedding
cosine similarity
the subwords aligned to each parser span
all offsets used in the alignment
```

The first test set should contain examples covering:

- single-token pronoun and single-token proper-name antecedent;
- multi-token antecedent;
- parser/model tokenization mismatch;
- two formally admissible antecedents;
- an NLI conclusion licensed by exactly one resolution;
- at least one non-English example.

## Decision rule for progression

The stages are diagnostic rather than commitments:

1. Establish whether raw contextual cosine improves over formal recency.
2. If not, test whether an explicitly extracted gender direction yields a targeted signal.
3. If feature directions are inadequate, train a small classifier with frozen XLM-R.
4. Test learned projections or partial unfreezing.
5. Fine-tune the complete encoder only when simpler approaches and learning curves justify the additional complexity.

Every stage should preserve the same formal candidates, provenance mechanism, NLI evaluation items, and data splits so that improvements can be attributed to the neural component.
