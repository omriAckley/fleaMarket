This is the newest version of the flea market simulator.

To see a demo run, go to example.clj and evaluate the whole file. If you're feeling adventurous, I would recommend altering something (for example, how clan1 is defined), and then re-evaluating.

Previous versions allowed for singular isolated simulations of one population, which made contextualizing results very difficult.
This version allows for simulations composed of multiple sub-populations (called "clans"), which interact on a flexible and user-defined schedule.

This new version also has changed from mostly immutable data to mostly mutable data.
As such, changes that used to be ephemeral are now conserved.