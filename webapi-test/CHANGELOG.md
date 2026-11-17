# Revision history for webapi-test

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
