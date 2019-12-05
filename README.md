# haskell-web-stack

A Haskell web application, built using dead-simple libraries:

* [`Spock`](http://hackage.haskell.org/package/Spock), for routing and handling HTTP requests
* [`blaze-html`](http://hackage.haskell.org/package/blaze-html), for generating HTML
* [`configurator`](http://hackage.haskell.org/package/configurator), for handling configuration
  files
* [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple), for DB access
* [`fast-logger`](http://hackage.haskell.org/package/fast-logger), for logging

This is meant as a didactic exercise, showing that much of the fancy machinery
often talked about in Haskell communities isn't necessary to build a functioning,
nontrivial web application.

What is the actual application? A website where users can set up their own timers and notes.
An example use case might be cooking: Someone might need to set up various different timers
to track the progress of different reagents, as well as notes to themselves about things
that they have to be careful of, things that can be improved for the next time they make
a recipe, etc. Another use case might be someone playing a MOBA, like League of Legends
or Dota 2, where they could have a page open in a second monitor to track key cooldowns,
as well as notes to themselves about how to macro versus the enemy composition and
cooldowns to keep in mind while teamfighting.

## Building and running

To build, you'll need to install Stack, npm, and npx. You'll also need `libpq` installed
for Postgres access. I assume you have GNU Make.

Create a Postgres database according to the tables in `tables.sql`, and update
`server-configuration.cfg` appropriately.

Run `make build`.

After that, you should be good to go. Run `make run` to start the server and navigate
to `localhost:3000` to see the application running.

## An invitation

While this application *works*, many parts of it would be considered unidiomatic
for production Haskell. For instance, many Haskellers would likely use Servant
instead of Spock for defining the API endpoints. If you're interested in
learning more of the advanced parts of Haskell, why not try refactoring and
upgrading this application with different libraries?

* Upgrading the DB access to use a type-safe query library instead of `postgresql-simple`.
  I recommend [Opaleye](http://hackage.haskell.org/package/opaleye)!
* Upgrade the API definition to use [`Servant`](https://docs.servant.dev/en/stable/)
  instead of Spock.
* Adding automated testing using [`QuickCheck`](http://hackage.haskell.org/QuickCheck) or
  [`hedgehog`](http://hackage.haskell.org/hedgehog). For instance, you could test the
  property that every error response from the server also sends back a JSON error
  message.
* Upgrading the frontend code to use [`PureScript`](http://www.purescript.org/) or
  [`Elm`](https://elm-lang.org/) instead of vanilla JavaScript.
* Upgrading the build system to use [`Shake`](http://hackage.haskell.org/package/shake)
  instead of Make to make things more robust.
