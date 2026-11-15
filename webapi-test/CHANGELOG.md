# Revision history for webapi-test

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
