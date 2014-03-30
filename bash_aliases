type __git_ps1 >/dev/null
if [ $? -eq 0 ]; then
   PS1="\u@\h:\w\$(__git_ps1) \$ "
fi
