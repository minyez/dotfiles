lazydata="$HOME/.local/share/nvim/lazy"
packdata="$HOME/.local/share/nvim/packer/start"
if [[ -d "$lazydata" ]]; then
  datadir="$lazydata"
else
  datadir="$packdata"
fi

repos=(
	"folke/which-key.nvim"
	"lewis6992/gitsigns.nvim"
	)

for repo in "${repos[@]}"; do
	dest="$datadir/${repo##*/}"
	if [[ -d "$dest" ]]; then
		echo "$dest exist, pass"
	else
		if which git > /dev/null 2>&1; then
			git clone "https://github.com/$repo.git" "$dest" 
		fi
	fi
done
