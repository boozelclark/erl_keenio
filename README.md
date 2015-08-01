#Erlang Keen IO Client

This is a very early, rough Erlang Keen IO client which can be used to send events from an Erlang application to Keen IO

##Usage
erl_keenio is built using [rebar3](http://www.rebar3.org/) which is included, else just add it as a dependency in your rebar.config file if you're already using rebar3.

Set the `project_id` and `write_key` environment variables in the `erl_keenio.app.src` file or override them if used as a dependency so they match your Keen IO project.

Make sure that the erl_keenio application is running.

To add a Keen IO event call `keenio:add_event(event_category,{some_event_truple, some_value}).` To add multiple events call  `keenio:add_events(SomeEventsTruple).``

You can periodically report VM statistics to Keen IO by calling `keenio:report_periodically(Seconds, Parameters).` where Parameters can currently be the atom `all` or a list of specific options which currently only supports `[memory]`. In future more options will be added.

You can read the Keen IO [data collection page](https://keen.io/docs/data-collection/) and [data modeling guide](https://keen.io/guides/data-modeling-guide/) to get an idea of the event structure. All input is parsed by [jiffy into](https://github.com/davisp/jiffy) JSON to be sent to Keen IO so what ever input you give the client it must be jiffy parseable into the Keen IO format.

##Why
There are some really good metric systems available for Erlang but with most of them the visualization of those metrics can be complex. You normally need to send them to Graphite or Riemann which are other systems you need to maintain.

When I saw Keen IO's [data explorer](https://keen.io/blog/114588771746/introducing-data-explorer) pop up on hacker News it looked like the perfect way to visualize metrics/events so i built this to give it a try. You can also create [dashboards] (http://keen.github.io/dashboards/) pretty easily.

##Contribute
Pull and Fork away.
