for DIR in $(ls -d */); do
    rm -r "${DIR}bin" "${DIR}obj"
done
