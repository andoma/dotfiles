if [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
   source /usr/local/etc/bash_completion.d/git-prompt.sh
fi

if [ -f /usr/lib/git-core/git-sh-prompt ]; then
   source /usr/lib/git-core/git-sh-prompt
fi

if [ -f /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash ]; then
    . /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash
    . /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh
fi

PATH="$HOME/bin:$PATH"

PS1="\u@\h:\w\$(__git_ps1) \$ "
