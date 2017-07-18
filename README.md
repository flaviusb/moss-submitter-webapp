# moss-submitter-webapp

A simple webapp that submits zip archives to MOSS for plagiarism checking.

It is written in Haskell, and uses stack as a build tool.
To set up the project, run `stack setup` in the project directory.
Use stack to build it: `stack build`.

Installing yesod is the easiest way to run the app; you can install it through stack by running `stack build yesod-bin cabal-install --install-ghc` in the project directory.

Then you can run a dev server like so: `stack exec -- yesod devel`.

Set the environment variable `$MOSSUserID` to your Moss user id before you run the program.

