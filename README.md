# devil
  Small proof-of-concept for a kind-of "daemon manager" scripted using lua

## Config file
  the config file is written using YAML
  
  any kind of configuration possible should be documented in /devil.conf.example

## Small Notes
  * Daemons must be enabled explicitly in the config file
  * Every daemon runs in its own thread
  * This is just a draft/demo of what a friend had in mind, everything here may change at anytime.

## Config
  there are two "main" scopes
  
  the global scope, which is from where default values may get taken (if needed)
  and the local scope, which is defined in a per-daemon basis, and may add or override (= take precendence over) the
  global scope

## Daemon details

Every daemon's minimal configuration
  * DAEMONS/name/daemon.conf
  
it has the following settings:
  * entry_point  (lua_file)

    defines the 'starting' point of the daemon.

    you probably want it to be `init.lua`, thus making the initial file loaded by Devil DAEMONS/name/init.lua

every daemon must register itself with the system, using `daemon.register`, it takes two parameters
the first is the type of daemon, the second are options to that daemon type
  
### daemon types:
   * daemon.dummy

       options: 

          Nothing
   * daemon.busy_wait

       config_settings:

          wakeup: int

             time between checks to `condition`
             (may not be accurate if condition is expensive and takes too much time)

       options:

          condition:

              specifies the condition which needs to be valid for the event/action to run.
              iow, if it returns true, action is called

          action:
              see condition

   * daemon.loop

       options:

          loop:

            function to keep running (it'll get called again the moment you quit)

### availables APIs
#### config

* config.get_integer (string) : maybe int

  tries to gather setting from config, may return nil if the value is not found (or doesn't have the right type)

* config.get_string  (string) : maybe string

  same as get_integer, but with string
 
#### chan
 * chan.read (string) : lua_value

   gets a primitive value from the named channel (shared over all daemons)
   it'll always return a value, iow, it waits for it

 * chan.write (string, lua_value) : unit

   sends a value over a named channel, non-locking
 
 note: lua_value is basically a way to say the value is either an integer, a string, a boolean or nil 

#### top-level

* sleep(n:int) : unit

  sleep for N seconds
* print(string) : unit

* throw_exception(err:string) : 'a

  (this function basically 'crashes' the current thread)

  
  
    
