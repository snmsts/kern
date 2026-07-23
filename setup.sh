#!/bin/sh
# setup.sh -- fetch the vendored dependencies Kern needs.
#
# vendor/ is not in the repo (it holds third-party code with its own history
# and license), so a fresh clone needs these two things:
#
#   vendor/cl-pdf  : cl-pdf with two fixes Kern's PDF backend needs, from the
#                    snmsts/cl-pdf fork (local-fixes branch). See DESIGN.md;
#                    upstream PRs mbattyani/cl-pdf#47 and #48. Once those merge
#                    the Quicklisp cl-pdf works and this clone is unnecessary.
#   vendor/jlreq   : jfm-jlreq.lua — the character-class and spacing data Kern
#                    reads at run time, from abenori/jlreq (BSD-2, Noriyuki Abe).
#
# Idempotent: skips whatever is already present. Needs git and curl.
#
# The core engine and tests need neither of these — (asdf:test-system "kern")
# runs on a bare Lisp. They are only for the PDF demos.

set -e
cd "$(dirname "$0")"

# --- cl-pdf (patched fork) -------------------------------------------------
if [ -f vendor/cl-pdf/cl-pdf.asd ]; then
  echo "vendor/cl-pdf: already present"
else
  echo "vendor/cl-pdf: cloning snmsts/cl-pdf (local-fixes)"
  git clone --branch local-fixes --single-branch \
    https://github.com/snmsts/cl-pdf.git vendor/cl-pdf
fi

# --- jlreq JFM data (jfm-jlreq.lua, BSD-2) ---------------------------------
mkdir -p vendor/jlreq
base="https://raw.githubusercontent.com/abenori/jlreq/master"
for f in jfm-jlreq.lua LICENSE; do
  if [ -f "vendor/jlreq/$f" ]; then
    echo "vendor/jlreq/$f: already present"
  else
    echo "vendor/jlreq/$f: fetching from abenori/jlreq"
    curl -fsSL -o "vendor/jlreq/$f" "$base/$f"
  fi
done

echo
echo "done. run the demos with:"
echo "  ros run -- --load load.lisp"
echo "  (kern::run-document-pdf)"
