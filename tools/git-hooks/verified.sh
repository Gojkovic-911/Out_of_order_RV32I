# Verification seal shared by verify.sh and the git hooks
# key: hash of a git tree without *.md files (documentation changes keep the seal)

SEAL="$(git rev-parse --show-toplevel)/out/verify/verified"

tree_key() {
    git ls-tree -r "$1" | grep -v -E '\.md$' | sha1sum | cut -d' ' -f1
}

# Tree of the working directory as it would be committed (tracked + new, .gitignore respected)
work_tree() {
    local idx
    idx=$(mktemp)
    cp "$(git rev-parse --git-dir)/index" "$idx" 2> /dev/null
    GIT_INDEX_FILE=$idx git add -A > /dev/null 2>&1
    GIT_INDEX_FILE=$idx git write-tree
    rm -f "$idx"
}

sealed() {
    [ -f "$SEAL" ] && grep -qx "$1" "$SEAL"
}
