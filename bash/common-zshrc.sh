# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="candy"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

RED='\033[0;31m'
NC='\033[0m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'

function progressbar() {
  if [ $# -ne 2 ]; then
    echo "Usage $0 <current_val> <total_val>"
    return 0
  fi
  count=$1
  total=$2
  pstr="[=======================================================================]"
  pd=$(( $count * 73 / $total ))
  printf "${BLUE}%3d.%1d%%${NC} ${GREEN}%.${pd}s${NC}\r" $(( $count * 100 / $total )) $(( ($count * 1000 / $total) % 10 )) $pstr
  if [ $count -eq $total ]; then
    echo
  fi
}


alias tl='tmux -2 attach -t laptop -d || tmux -2 new -s laptop'
alias tl1='tmux -2 attach -t laptop1 -d || tmux -2 new -s laptop1'
alias tl2='tmux -2 attach -t laptop2 -d || tmux -2 new -s laptop2'
alias tl3='tmux -2 attach -t laptop3 -d || tmux -2 new -s laptop3'
alias td='tmux -2 attach -t desktop -d || tmux -2 new -s desktop'
