# moss-submitter-webapp

A simple webapp that submits zip archives to MOSS for plagiarism checking.

It is written in Haskell, and uses stack as a build tool.
To set up the project, run `stack setup` in the project directory.
Use stack to build it: `stack build`.

Installing yesod is the easiest way to run the app; you can install it through stack by running `stack build yesod-bin cabal-install --install-ghc` in the project directory.

Then you can run a dev server like so: `stack exec -- yesod devel`.

Set the environment variable `$MOSSUserID` to your Moss user id before you run the program.

There is also a script `kick-maker.sh` which you can run with your MOSSUserID to generate a `kick.sh`, which you can add to your crontab with something like:
```
20 * * * * /home/myusername/code/moss-submitter-webapp/kick.sh
```
This would check whether there was a screen session with the name mosssubmitterwebapp 20 minutes past the hour, every hour, and if not it would create such a session and run `stack exec -- yesod devel` inside it.
