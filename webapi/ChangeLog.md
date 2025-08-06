# Changelog

## 0.6
* Use `Control.Exception.Safe` everywhere instead of `Control.Exception` from `base`.
* Lift `client` to work over `MonadUnliftIO m` instead of `IO`.
* Added support for GHC 9.6

## 0.5
* Added support for GHC 9.2

## 0.3
* WebApiImplementation typeclass is renamed to WebApiServer
* Swapped the order of FromParam and ToParam class parameters.
* Fixed TmpFileBackend for file upload.
* Added fieldModifier to ParamSettings.
* Added support for cookie attributes.
* Fixed RequestBody content type matching.

## 0.2.2
* Added support for GHC 8

## 0.2.1

* Added `RequestBody` to the `Request` type. This allows user to have content in request's body with the desired `Content-Type`.
* Added `Request` pattern synonym.
* Added `Field` data type for aliasing field names.
