# Ecosystem and scope

`collection-json` encodes, decodes, and manipulates
`application/vnd.collection+json` documents—and nothing more. It gives
you the envelope types the spec defines, their `ToJSON`/`FromJSON`
instances, and the `FromCollection`/`ToCollection` contract for moving
your own domain types through that envelope. This document marks where
that job ends and what lives beyond it.

## The envelope isn't the pattern

A Collection+JSON document is one message on the wire. A working
hypermedia API needs more than the ability to serialize it: it
negotiates which representation to send, publishes the semantics a
client acts on, validates the writes a client sends back, and manages
how those shapes change over time. Each is a distinct concern with its
own standards and failure modes.

None belongs in this library. Serializing the envelope is stable and
small—it tracks one specification and changes only when that
specification does. The surrounding concerns are framework- and
deployment-specific, and move at their own pace. A library that owned
them all would couple decisions that needn't be coupled, and every
consumer would pay for parts it didn't use. A separate package per
concern lets each be chosen, or omitted, on its own.

## The adjacent layers

**URI values.** Every link is a URI, and this library models link
fields as `Network.URI` rather than text. That pushes URI serialization
to a neighbor: [`network-uri-json`][network-uri-json] supplies the
`ToJSON`/`FromJSON` instances and composes directly with the types
here. Same author, for this reason.

**Content negotiation.** A Collection+JSON response can carry a profile
parameter that narrows a generic media type ([RFC 6906][rfc6906]).
That's a property of the HTTP exchange, not the document, so it belongs
in the web framework serving the response. No Haskell library packages
it—it's a few lines of header handling where routing already lives.

**Profile semantics.** A profile parameter names a profile; something
still has to define what it means—the link relations, the permitted
actions, the state transitions. [ALPS][alps] (Application-Level Profile
Semantics) is one vocabulary for that. A profile description is data a
client fetches and interprets, served as its own resource, so it sits
outside the envelope by design. No Haskell library models ALPS today.

**Request validation.** A response advertises a `template` for a write,
and the client sends data back against it. Validating that shape is a
job for [JSON Schema][json-schema] 2020-12, using any existing Haskell
implementation—a companion to the response types, not a dependency of
them.

**Schema compatibility.** Request and response shapes evolve, and
evolving them without breaking clients—who may hold different roles and
see different fields—is its own discipline. [jsoncompat][jsoncompat]
audits schema changes along those lines. No Haskell port yet; the
concern is real even where the tooling isn't.

## Why the edge holds

Any of these could be pulled into this package. None should be. A
wire-format primitive earns its keep by doing one legible thing and
staying out of the way of what an API builds on top. When widening it
tempts—to teach it profiles, negotiation, validation—the answer is a
new package that depends on this one, not a new responsibility inside
it.

[alps]: https://datatracker.ietf.org/doc/draft-amundsen-richardson-foster-alps/
[jsoncompat]: https://github.com/ostrowr/jsoncompat
[json-schema]: https://json-schema.org/
[network-uri-json]: https://github.com/alunduil/network-uri-json
[rfc6906]: https://www.rfc-editor.org/rfc/rfc6906
