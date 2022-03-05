# Feedback loop

* General feedback loop system for arbitrary files and commands.
* I want to have a good idea of the current state of things:
  * Is something running or not?
  * How many runs are queued?
  * Is it blocking on CPU, on memory, on network?
* Clear previous feedback next time.
* Make it possible to queue feedback and cancel the previous one.
* Low latency between change and rerun.
* Cancelling failed feedback loops from before.
