# chicken-gochan

 [Chicken Scheme]: http://call-cc.org/
 [Go]: http://golang.org/

[Go]-inspired channels for [Chicken Scheme]. Essentially thread-safe
fifo queues that are useful in concurrency and for thread
synchronization. This implementation has largely been inspired by
[this Go channel tutorial](https://gobyexample.com/channels).

chicken-gochan has no egg dependencies.

## Development Status

Currently supported:

- closable channels (they can have limited length)
- receive from multiple channels `(gochan-receive (list channel1 channel2))`
- receive timeouts
- `gochan-select` syntax

## Comparison to real Go Channels

- all channels have an unlimited buffer
- types are dynamic

## API

    [procedure] (make-gochan)

Construct a `gochan` record. Each gochan holds a buffer whose length
is limited by memory only.

    [procedure] (gochan-send chan msg)

Add msg to chan's message buffer. This will "awaken" a single thread,
if any, waiting for messages on chan. It is an error to send to a
closed channel.

    [procedure] (gochan-receive chans [timeout/seconds])

`chans` is a single gochan or a list of gochans. Pops next msg from
any of chans' buffers in a thread-safe manner, if possible. Otherwise,
if channel is empty, wait for a `gochan-send` from another thread. If
timeout is supplied and is reached, or all channels are empty and
closed, an error is thrown.

If multiple channels in chans have messages available, the message
from the first channel is popped first.

If you have multiple threads waiting messages from the same gochan,
the order in which threads receive messages is unspecified (but each
message is received only once).

    [procedure] (gochan-receive* chans timeout/seconds)

Like `gochan-receive`, but doesn't error out. Instead, it returns:
`#f` for all channels closed, `#t` for timeout and `(list <msg>)` on
success (distinguishable from a `#f` or `#t` message).

    [procedure] (gochan-close chan)

Close channel. It is an error to send to a closed channel. It is an
error to receive from an empty and closed channel.

    [procedure] (gochan-closed? chan)

Closed predicate.

    [procedure] (gochan-for-each chans proc)

Call `(proc <msg>)` for each msg in chan (`proc` must unfortunately
have side-effects). Returns `(void)` when chan is closed.

    [procedure] (gochan-fold chans proc)

Like a normal fold, but fold over chans's messages.

    [syntax] (gochan-select (<chan> <var> body ...) ... (<timeout/seconds> body ...))

Convenience syntax for handling incoming messages from different
gochans differently. Used as in [Go], typically:

```scheme
(gochan-select
 (chan1 msg (error "from c1" msg))
 (chan2 obj (list  "from c2" obj))
 (1 (error "waited one second, but got nothing!")))
```

It is an error to specify multiple timeouts. `gochan-select` returns
the associated channel's body return-value.

## Samples

See `./tests/worker-pool.scm` for a port of
[this Go example](https://gobyexample.com/worker-pools).
