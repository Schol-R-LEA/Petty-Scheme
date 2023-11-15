for filepath in tests/*.scm; do
    [ -e "$filepath" ] || continue
    filename=$(basename $filepath .scm)
    echo $filename
    make $filename
    bin/$filename
done