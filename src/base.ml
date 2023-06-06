type effect = Effect : 'a Effect.t -> effect

exception Outside of effect
