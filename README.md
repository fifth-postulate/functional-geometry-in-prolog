# Functional Geometry in Prolog
This project contains an exploration of the ideas found in the paper ["Functional Geometry"][henderson] by Peter Henderson, transposed to Prolog.

## Functional Geometry
I learned about the paper and it's contents in a workshop by [Einar Høst][host] called: [Escher in Elm][escher-in-elm].

It is a wonderfull workshop that clearly explains the ideas of functional geometry. I.e.

> that one could write an algebraic description, embed it in a functional program, and execute it directly

## Prolog
These ideas can be expressed in various other languages as Einar Høst and others have shown. Examples include [F#][f#], [PostScript][postscript] and [Rust][rust].

This repository explores the ideas in [Prolog][swi-prolog].

Prolog seems well-suited to express the ideas of functional geometry.

## Usage
Start SWI-Prolog and load the `geometry.pl` file with the following command.

```sh
swipl geometry.pl
```

At the query prompt enter the following query.

```prolog
escher(2, C), processTo('output.svg', C).
```

Where `output.svg` is the file you want to write the result to.

<img alt="Eschers level 2 Square Limit by Prolog" width="500px" height="500px" src="http://fifth-postulate.nl/functional-geometry-in-prolog/image/escher2.svg">

[henderson]: https://eprints.soton.ac.uk/257577/1/funcgeo2.pdf
[host]: https://twitter.com/einarwh
[escher-in-elm]: https://github.com/einarwh/escher-workshop
[postscript]: https://www.lambdadays.org/lambdadays2020/einar-host
[f#]: https://www.lambdadays.org/lambdadays2018/einar-host
[rust]: https://github.com/fifth-postulate/esche.rs
[swi-prolog]: https://www.swi-prolog.org/
