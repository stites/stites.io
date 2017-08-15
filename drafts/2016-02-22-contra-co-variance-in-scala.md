So I think the idea is that, in scala, you can state contra- and co- (and in-)
variance without establishing a concrete type. So you would use this only in
parameterized types, for the parameters themselves - eg: the option type.

For this reason, only use `+`/`-` notation for parameterized types (not the thing i
sent you); in contrast you would use bounded types in the same way java uses contra-
and co- variance. Meaning that with bounded types you can pass around some "generic"
(in the java sense) in your method/class/function, etc.

