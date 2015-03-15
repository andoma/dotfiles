if [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
   source /usr/local/etc/bash_completion.d/git-prompt.sh
fi

type __git_ps1 >/dev/null 2>/dev/null
if [ $? -eq 0 ]; then
   PS1="\u@\h:\w\$(__git_ps1) \$ "
fi
