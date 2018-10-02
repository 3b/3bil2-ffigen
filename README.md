## FFI definition generator for Android API for 3bil2 (a CL-like lisp compiling to dalvik bytecode)

Parses contents of `android.jar` from the android SDK at `READ` time,
so you need to copy or symlink that into the 3bil2-ffigen
directory. Tested with version 28, but hopefully works with other
versions.

(If you just want the `.jar` file without whole SDK, they are also in
the
`https://android.googlesource.com/platform/prebuilts/sdk/+/master/`
repo. You can get single files with something like `curl "https://android.googlesource.com/platform/prebuilts/sdk/+/master/28/public/android.jar?format=TEXT" | base64 -d > android.jar` where `28` can be replaced by other version number or `current`.)
