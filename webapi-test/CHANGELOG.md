# Revision history for webapi-test

## 0.7.0.0 -- 2026-11-19 (zenbridge #2715: what a move's host is told, and what it may say)

Four things a scenario's host could not do, none of which names a host's
own ideas. All are breaking for a host that builds its `MoveHost` with the
constructor: build it from `defMoveHost` (`defMoveHost {settle = …,
observe = …}`), and a field added later breaks nothing.

* **The host is handed a move as it was played** (`MovePlayed`: its name,
  where it was written, its own steps in order). `settle` and `observe`
  take it as their first argument (breaking), and so does `verdict`.
  * A step is a `MoveStep`: its kind, where it was written, its operation,
    a call's request as it went out (`Sent`) and what the step gave
    (`Answered`). These are the values the engine held, not the trace's
    text: nothing is rendered, clipped or masked. A host reads them by a
    cast to its own types, `sentAs @POST @SomeRoute step` and
    `answeredAs @SomeAnswer step`.
  * The steps are the move's calls, its effect steps (a foreign transform,
    a host's own step: no request, the result as its value) and its fresh
    values. Its begin and settle steps, a context switch, a step that only
    moves the model and a model's extended action are not among them, nor
    is any step outside the move.
  * No recorder is needed. A step that fails ends its case before the
    move's settle step, so every step handed over has its value.
* **The host has its word on a move once the checks have run**
  (`verdict :: MovePlayed -> SomeObservation -> [CheckRecord] ->
  WebApiSessions apps MoveVerdict`): it sees the move, what was observed
  and every check's record, and answers a reason the move is inconclusive
  and the evidence it noted.
  * **The engine keeps the order.** A check that failed fails the case, and
    the host's reason stays with the failure. When every check held and
    the host gave a reason, the move is inconclusive: its case is
    discarded, as one that did not settle is.
  * The settle step of such a move keeps what its checks left (each check,
    the labels, the model and the stage after it) and is marked
    `inconclusive`. A move inconclusive before its checks (`settle` or
    `observe` answered `Left`) has no check of its own, as before.
  * A regression case that ends this way on replay reads `inconclusive`,
    with the reason: never `passed`.
* **The host attaches evidence to a move** (`Evidence`: a `kind` of the
  host's own, one line `what`, an optional JSON `detail` carried as it
  is). Evidence changes no outcome.
  * It is kept on the move's settle step (`StepRecord.evidence`, breaking:
    a new field; `"evidence"` in its JSON), so it goes wherever the step
    goes: a sink, the first failure, the narrowed case, a replay, a cut
    case.
  * A failing move's evidence is listed in the failure's text, under its
    failed checks (`evidence, <kind>: <what>`). A reason the host gave
    when a check failed is kept as one more item, of the engine's own kind
    `otherwise inconclusive`.
  * The run's trace counts it by kind over the generated cases that
    counted (`RunTrace.evidence`, breaking: a new field): how many cases
    and moves carried it, and the first three in full. An inconclusive or
    discarded case counts nothing, as for the labels and the moves.
  * The record bounds it (`boundEvidence`, `evidenceLimit`): a line is
    clipped at 4,000 characters, a detail over 32,768 bytes as JSON is
    replaced by `{"dropped": bytes}`, and a move keeps 100 items, the rest
    counted in one last item of the kind `evidence not kept`.
* **The host is told that a case begins** (`caseBegins`): once before the
  first step of every play, a generated case, a narrowing try and a
  regression case replayed alike.
* `newWebApiSessionsCxt` replaces `initWebApiSessionsCxt` and `tracingCxt`
  (breaking): a case's context holds the move it is in, so one is made for
  each play.
* A run that uses none of this costs what it did. The 0.6.0.0 `moves`
  suite, its host ported by two lines, allocates +0.09% at -O0 and -0.11%
  at -O2 against 0.6.0.0.

## 0.6.0.0 -- 2026-11-18 (zenbridge #2741: a recorder's sink)

* **A recorder hands what it records to a sink, as it happens**
  (`newRecorderTo`, `TraceEvent`), so a run can be followed while it plays.
  * `StepEnded phase step`: each step once, when it is final. That is as
    soon as its checks have run (`sealStep`, which the state model calls
    after a step's postcondition, or after its inconclusive mark), not when
    the next step is pushed: a move's own step is handed over before its
    settle step begins. A step whose checks never ran (it failed, or its
    case was cut) is handed over as its case closes.
  * `CaseEnded phase close`: a play closed, after its last step
    (`CaseClose`: its outcome, the step it failed at or reached, why, its
    moves, the labels that held). A case QuickCheck discards closes too.
  * `Phase` tells a replayed regression case, a generated case and a
    narrowing try apart (`{"replay": 1}`, `{"case": 2}`, `{"try": 3}`).
  * The sink is called on the thread that plays the case, with asynchronous
    exceptions masked, so an event is handed over whole or not at all: it
    must neither block nor throw (put the event on a queue). A recorder
    without a sink records as before.
* **A regression case cut while it replays is recorded** (breaking:
  `ReplayOutcome` gains `ReplayCut`). `replayCasesDL` marks the replay it
  is in (`beginReplay`); `closeCut` notes it among the replays as `cut`,
  with the steps it had made, and `RunTrace.cut` holds it (`replay k`).
* **A regression case is replayed once.** A replay QuickCheck discarded (a
  move that did not settle) was played again, up to ten times: the same
  case each time.
* `reached`: where a case stopped while it was open got to (its last step,
  and the move it was in).

## 0.5.0.0 -- 2026-11-17 (zenbridge #2730: a run cut, and a run's budgets)

* **A cut case is recorded** (breaking: `CaseOutcome` gains `Cut`,
  `RunTrace` gains `cut`). A run stopped while a case is open, by its time
  or allocation limit or its thread cancelled, never reaches QuickCheck's
  end-of-case callback, so the case used to vanish from the trace.
  `closeCut rec why`, called once the run has stopped, closes it from the
  steps already pushed: as `cut`, at its last step (the move it reached),
  its moves ending with the one it was in, its steps in `RunTrace.cut`.
* **Allocation budgets, a case's and a move's** (`Test.WebApi.Budget`).
  `withCaseBudget AllocBudget {perCase, perMove}` around the runner that
  plays a case arms GHC's allocation limit on the case's thread; each move,
  as it begins, gets its own budget or what the case has left, whichever is
  less. A case or move over budget gets `AllocationLimitExceeded`, which
  nothing on the way catches. It counts the case's own thread: a host that
  runs an application in process puts that work on a thread of its own.

## 0.4.0.0 -- 2026-11-16 (zenbridge #2714: a scenario's moves, stages and checks)

* **A fresh value is new each time it is drawn.** `FreshValue` begins a step
  of its own, so the session's one-token-per-label-per-step cache no longer
  hands a second draw of the same label the first draw's token (two things a
  move made in one case had one "fresh" PO).
* **`EffectCall` runs in the sessions** (breaking). Its effect is
  `LookUp -> WebApiSessions apps (Either Text res)`, was `IO`: a host's
  own step (a document built from a spec and uploaded, several calls) runs
  as the run's context. An `IO` effect lifts with `liftIO`.
* **A scenario's moves** (`Test.WebApi.Moves`). A move is one action by an
  actor, written as DL: its things picked by the stage they are predicted
  at (`pickAt`), a step that begins it (`MoveBegin`), its own steps, and a
  step that ends it (`MoveSettle`). `anyMoves n` draws moves one after
  another, `n` on average, each by its weight among the moves allowed at
  that point (quickcheck-dynamic's own weighted choice).
* **The prediction** (`Test.WebApi.Prediction`). `ApiState` gains
  `prediction`: the things a scenario tracks (`Thing`: its track, the stage
  it is predicted at, the moves made on it, the labels its draws gave, its
  value). A `Track` declares a kind of thing's stages, its refusal stages
  and the platform's answers, the paths it takes by itself; the first
  answer whose `when` holds is taken. A thing at a stage is also recorded
  under the class `track@stage`.
* **A move's end is judged against what the host observes.** The host
  settles what the move caused within the move's budget and observes it
  (`MoveHost`, in `WebApiGlobalStateModel`'s new `appMoveHost`;
  `defMoveHost` settles at once and observes nothing). The property's
  `Judge` then holds every thing against the observation: its observed
  stage against the predicted one, the checks at that stage, the checks
  after the move for the things it touched, and the checks across things.
  The host's own checks run too. A failure names the check and the thing.
* **Inconclusive.** A move that does not settle within its budget, or
  cannot be observed, stops its case: neither passed nor failed.
  QuickCheck discards the case. The trace says `inconclusive` and why
  (`CaseOutcome.Inconclusive`, `ReplayOutcome.ReplayInconclusive`,
  `StepRecord.inconclusive`).
* **Narrowing through the program** (`Test.WebApi.Replay.narrowThroughProgram`,
  used by `propDL` and `propDLTraced`). quickcheck-dynamic keeps a shorter case's own
  steps and checks them against the program only by `Eq`, which for a call
  compares its operation and label, so a step under a narrowed pick kept
  the request it was drawn with. Each shorter case is now made again from
  its picks through the program, as a regression case is replayed. Every
  pick is offered narrowed too: quickcheck-dynamic narrows a run of picks
  only from its first. A move is kept or dropped whole. `forAllDLRecorded`
  takes the narrowing as its first argument.
* A move's body reports what it gave (`MoveBody`: the values of the things
  it makes, the labels its draws said, and the variables those values use
  beyond their own, kept on the settle step, `MoveEnd.uses`, so narrowing
  never keeps a move whose things refer to a step it dropped).
* The trace: step kinds `begin` and `settle`; `StepRecord.inMove` (the move
  a step belongs to); `CheckRecord.subject` (the thing a check was about);
  a case's `moves`, and the move counts, list a scenario's moves rather
  than their steps.
* `AnyVal` moves to `Test.WebApi.Val` (still re-exported by
  `Test.WebApi.StateModel`).
* A test suite, `moves`: two moves against a toy platform that does not
  acknowledge a replacement 850.

## 0.3.0.0 -- 2026-11-16 (zenbridge #2713: regression cases, with exact replay)

* **A run's narrowed case, written out** (`Test.WebApi.Case`): the case
  quickcheck-dynamic drew, its picks and steps in order. A step the
  program writes is kept by its kind, trail and operation, and the replay
  makes it again from the program. A drawn step (`anyActions`) is kept by
  its operation, the positions its generation drew, and the earlier steps
  its request refers to. A fresh value is never kept: it is made again,
  and every step that shared it still shares it. The trace's
  `narrowed.case` and `first_failure.case` hold it, as JSON.
* **Replay** (`Test.WebApi.Replay`): `replayTest` walks the program's
  formula, giving each quantifier the case's next pick and checking each
  step against the case's next move. It says why when the program no
  longer plays the case: a move gone or changed, a pick of another type,
  a drawn move the state no longer offers. `replayCasesDL` plays each
  case once, before the property draws its own, and records how it fared
  (`RunTrace.replays`: passed, failed at a move, or `cant_replay` and why).
* `propDLTraced` records through `forAllDLRecorded`, quickcheck-dynamic's
  own generation and narrowing with each test handed to the property.
  It needs `runDL`, which byteally's quickcheck-dynamic 4.0.1.1 exports.
* Generation keeps the positions it draws (`ApiGenM`,
  `runApiGenMDrawing`), and can draw them again (`runApiGenMRedrawing`).
  A drawn action is stamped with its operation (`stampDrawn`).
  `WebApiGlobalStateModel` gains `appRedraw` (`noRedraw` for a model that
  never draws).
* `Val`: `varLeaves` and `repointVal`, the variables of a value by path;
  `GValRep` gains `gTraverseLeaves`.
* `Recorder`: `newRecorderFor` (the program's version and the property's
  name, for the cases it writes out), `beginCaseWith`, `takeSteps`,
  `noteReplay`, `stepsFailure`.

## 0.2.0.0 -- 2026-11-15 (zenbridge #2712: a run's trace)

* **A run's trace, as data** (`Test.WebApi.Trace`, qc-dynamic). A
  `Recorder` keeps one record per step a case played: the request as sent
  (fresh values filled, credentials masked, the body clipped), the answer as
  received (status, body, the step's value), every check by name with
  whether it held, the labels that held, the model after the step (each
  class with its values) and the stage before and after. For the run
  (`RunTrace`): a line per generated case and per narrowing try (`taken`
  when it still failed), the first failing case and the narrowed one in
  full, and the label and move counts over the generated cases; JSON in
  snake_case. `propDLTraced` records a property; the cases close through
  QuickCheck's own callbacks (`PostTest` after every test, narrowing tries
  included; `PostFinalFailure` on the narrowed case). `propDL` records
  nothing, as before.
* **Checks by name.** A step's postcondition is a list of named checks
  (`andCheck`; `andPostcondition` adds one named `postcondition`,
  `ensuresRefinement`/`refutesRefinement` add `ensures <n>`/`refutes <n>`).
  Every check runs and every failure is reported; it was the first only.
* **Labels** (`andClassify`): a step gives its case a label where a
  predicate holds, through QuickCheck's `classify`; a label never fails.
* **Where a step was written** (`setStep`, `StepInfo`): a rule's trail,
  summary and operation name, for the trace; it also sets the label.
  `EffectCall` carries one (a new field, before the variables).
* **A model's stages**: `WebApiGlobalStateModel` gains `appStageOf`
  (`noStage` for a model with none).
* `WebApiSessionsCxt` carries the recorder (`tracingCxt`).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
