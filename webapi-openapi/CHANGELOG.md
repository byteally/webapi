# Revision history for webapi-openapi

## Unreleased

* The registry emits dhall-do-api's current names (`OperationId`,
  `mkOperationId`, `WebApi.Contract hiding (OperationId)`) and an
  `ErrorText` instance for every named error type.
* A modular layout, `openapi-model-generator --config FILE` (JSON; see
  `WebApi.OpenAPI.Modular`): one package per API, split into public
  sublibraries by consumer. `model-<m>` per module (from the document's
  `x-zb` object), the contract as the main library (`App`, `Routes.<M>`,
  `Contract`, `Contract.<M>`), and `registry` (`Registry.Instances.<M>`,
  `Registry.Ops.<M>`, `Registry`). Hand-written stanzas are appended from the
  config's `cabalExtra`; `gen/warnings.txt` lists what could not be typed.
* In the modular layout:
  * component names resolve once to PascalCase Haskell names, avoiding what
    the generated modules import;
  * an untyped or property-less schema is `Opaque` (its JSON, crossing to
    Dhall as text), never a crash;
  * optional arrays are `Maybe`, and `ToJSON` leaves absent fields out
    rather than sending `null` (an update touches only what it names);
  * `NoFieldSelectors`, and positional pattern variables in `ToJSON`;
  * a route with several captures gets a named path record (`<Op>PP`)
    instead of webapi's tuple, which dhall-do's bridge has no instances for;
  * header parameters are a record whose fields are the header names in
    snake case, with a ToHeader that sends each under its wire name and
    leaves an absent optional header out;
  * of several media types, JSON is kept; of several 2xx responses, the
    lowest; form and multipart bodies are left out with a warning until a
    plan can carry a file;
  * array request bodies of generated types get a whole-value
    `OverrideType` (until dhall-do-api has one for `Vector`).
* The legacy single-module layout (the flags) is unchanged; a golden test
  (`test/golden/ns-currency`) pins its output.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
