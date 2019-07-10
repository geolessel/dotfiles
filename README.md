# Setting up gpg to automatically sign commits without prompting for password

1. Install GPGTools - [https://gpgtools.org/](https://gpgtools.org/)

2. Transfer old or create new GPG keys

3. Configure git to look for these

    ```
    git config --global gpg.program /usr/local/MacGPG2/bin/gpg2
    git config --global commit.gpgsign true
    ```

4. Add this to ~/.gnupg/gpg-agent.conf

    ```
    pinentry-program /usr/local/MacGPG2/libexec/pinentry-mac.app/Contents/MacOS/pinentry-mac
    ```

5. Add this to `~/.gnupg/gpg.conf`
    ```
    no-tty
    ```
