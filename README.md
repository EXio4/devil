# devil
  Small proof-of-concept for a kind-of "daemon manager" scripted using lua

## Config file
  the config file is written using YAML
  
  any kind of configuration possible should be documented in /devil.conf.example

## Small Notes
  * Daemons must be enabled explicitly in the config file
  * Every daemon runs in its own thread
  * This is just a draft/demo of what a friend had in mind, everything here may change at anytime.

## Daemon details
  The minimal hierarchy is (with DAEMONS defined as the 'path' settings)
  * DAEMONS/<name>/init.lua
  
  it must define the following functions:
  * cond:
    
    parameterless function
    
    if this function returns:
    
       * `nil` or `false`, the action will not be evaluated
       * `true`          , the action will     be evaluated (without giving it any extra argument)
       * anything else   , the action will     be evaluated (giving it the return value as argument)
  * action
    
    (possibly parameterless, see cond)
    
    this should be the "worker" code, printing notifications, saving stuff to a log/file
    
    or doing whatever it needs to do (to be useful, maybe!)






