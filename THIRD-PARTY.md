# Third-party components

This repository's own code (src/, demo/, load.lisp, *.asd) is under the MIT
License; see LICENSE. The components below are bundled under `vendor/` and keep
their own licenses. Their copyright notices must be retained on redistribution.

## vendor/jlreq — BSD 2-Clause

- Copyright 2017-2024, Noriyuki Abe. All rights reserved.
- Upstream: https://github.com/abenori/jlreq
- Full text: `vendor/jlreq/LICENSE`

`src/jfm.lisp` reads `vendor/jlreq/jfm-jlreq.lua` at run time to build its
character-class and glue tables; it does not embed the table. The character
class numbers quoted in `jfm.lisp` comments are factual data. Redistribution
of the bundled `jfm-jlreq.lua` must keep the BSD-2 notice above.

## vendor/cl-pdf — FreeBSD-style (BSD 2-Clause) license

- Marc Battyani <marc.battyani@fractalconcept.com>
- Upstream: http://www.fractalconcept.com/asp/html/cl-pdf.html
- Full text: `vendor/cl-pdf/license.txt`
- Note: this repo uses the local `local-fixes` branch (see typeset.asd header).

Only `typeset/pdf` depends on cl-pdf; the `typeset` core has no dependency
on it.
