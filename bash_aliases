if [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
   source /usr/local/etc/bash_completion.d/git-prompt.sh
fi

PS1="\u@\h:\w\$(__git_ps1) \$ "
