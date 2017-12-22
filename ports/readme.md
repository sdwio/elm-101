# Port Usage

    1 the module has to declare that it is using ports: port module Main exposing (..)
        * use sparingly
    2 ports can be declared on
        * update -> send values to the js component `port requestNumbers : String -> Cmd msg`
        * subscriptions -> get input from the js part
