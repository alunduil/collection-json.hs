# Ecosystem and scope

`collection-json` encodes, decodes, and manipulates
`application/vnd.collection+json` documents. That's the whole of its
job. It gives you the envelope types the spec defines, their
`ToJSON`/`FromJSON` instances, and the `FromCollection`/`ToCollection`
contract for moving your own domain types in and out of that envelope.
It stops there on purpose, and this document explains where the edge is
and what lives on the other side of it.

## The envelope isn't the pattern

A Collection+JSON document is one message on the wire. A working
hypermedia API built around that message type needs more than the
ability to serialize it: it negotiates which representation to send,
publishes the semantics a client needs to act on a response, validates
the write requests a client sends back, and manages how those shapes
change over time. Each of those is a distinct concern with its own
standards and its own failure modes.

Folding them into this library would be a mistake. The serialization of
the envelope is stable and small—it tracks a single specification and
changes only when that specification does. The surrounding concerns are
framework-specific, deployment-specific, and move at their own pace. A
library that owned all of them would couple decisions that have no
reason to be coupled, and every consumer would pay for the parts they
didn't use. Keeping the envelope in its own package lets the adjacent
layers be chosen—or omitted—independently.

## The adjacent layers

**URI values.** Collection+JSON is full of links, and a link is a URI.
This library represents those fields as `Network.URI` values rather than
as text, which pushes the question of URI serialization to a neighbor:
[`network-uri-json`][network-uri-json] supplies the `ToJSON`/`FromJSON`
instances for `Network.URI` and composes directly with the types here.
It ships from the same author for exactly this reason.

**Content negotiation.** A client and server agree on a representation
through the `Content-Type` and `Accept` headers, and Collection+JSON
responses can carry a profile parameter that narrows the meaning of a
generic media type. [RFC 6906][rfc6906] describes that mechanism. It's
inherently a property of the HTTP layer, not of the document, so it
belongs in whatever web framework serves the response rather than in the
type that models the body. There's no standalone Haskell library for
it because there's little to package—it's a few lines of header
handling wherever routing already lives.

**Profile semantics.** A profile parameter names a semantic profile, but
something has to define what that profile means: which link relations
exist, what a client may do with them, what state transitions are
available. [ALPS][alps] (Application-Level Profile Semantics) is one
vocabulary for writing that down. A profile description is data a client
fetches and interprets, typically served over HTTP as its own resource,
so it lives outside the response envelope by design. No Haskell library
models ALPS today; the format is consumed as data when it's used at
all.

**Request validation.** Collection+JSON responses advertise a
`template` describing the shape of a write, and a client sends data back
against it. Validating that inbound shape is a schema-validation problem,
and [JSON Schema][json-schema] 2020-12 is the usual tool for it. Any
existing Haskell JSON Schema implementation covers this; it's a
companion to the response types, not a dependency of them, so the choice
stays with the application.

**Schema compatibility.** Once request and response shapes are in
production they evolve, and evolving them without breaking clients is its
own discipline—especially when different clients hold different roles
and may see different fields. [jsoncompat][jsoncompat] audits schema
changes for compatibility along those lines. There's no Haskell port
yet; it's noted here because the concern is real even where the tooling
isn't.

## Why the edge holds

Each layer above could, in principle, be pulled into this package. None
should be. The value of a wire-format primitive is that it does one
legible thing and stays out of the way of everything a real API layers
on top of it. When the temptation arises to widen the library—to teach
it about profiles, or negotiation, or validation—the answer is a new
package that depends on this one, not a new responsibility inside it.

[alps]: https://datatracker.ietf.org/doc/draft-amundsen-richardson-foster-alps/
[jsoncompat]: https://github.com/ostrowr/jsoncompat
[json-schema]: https://json-schema.org/
[network-uri-json]: https://github.com/alunduil/network-uri-json
[rfc6906]: https://www.rfc-editor.org/rfc/rfc6906
