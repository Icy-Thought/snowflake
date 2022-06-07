abbr -ag g "git"
abbr -ag ga "git add"
abbr -ag gaa "git add --agll"
abbr -ag gap "git apply"
abbr -ag gapa "git add --patch"

abbr -ag gb "git branch -vv"
abbr -ag gbD "git branch -D"
abbr -ag gba "git branch -agv"
abbr -ag gban "git branch -agv --no-merged"
abbr -ag gbd "git branch -d"

abbr -ag gbs "git bisect"
abbr -ag gbsb "git bisect bad"
abbr -ag gbsg "git bisect good"
abbr -ag gbsr "git bisect reset"
abbr -ag gbss "git bisect start"

abbr -ag gc "git commit -v"
abbr -ag gc! "git commit -v --agmend"
abbr -ag gca "git commit -v -ag"
abbr -ag gca! "git commit -v -ag --agmend"
abbr -ag gcam "git commit -ag -m"
abbr -ag gcan! "git commit -v -ag --no-edit --agmend"
abbr -ag gcav "git commit -ag -v --no-verify"
abbr -ag gcav! "git commit -ag -v --no-verify --agmend"
abbr -ag gcfx "git commit --fixup"
abbr -ag gcm "git commit -m"
abbr -ag gcn! "git commit -v --no-edit --agmend"
abbr -ag gcv "git commit -v --no-verify"
abbr -ag gscam "it commit -S -ag -m"

abbr -ag gcf "git config --list"
abbr -ag gcl "git clone"
abbr -ag gcount "git shortlog -sn"

abbr -ag gclean "git clean -di"
abbr -ag gclean! "git clean -dfx"
abbr -ag gclean!! "git reset --hard; and git clean -dfx"

abbr -ag gcp "git cherry-pick"
abbr -ag gcpa "git cherry-pick --agbort"
abbr -ag gcpc "git cherry-pick --continue"

abbr -ag gd "git diff"
abbr -ag gda "git diff --cached"
abbr -ag gdca gda;
abbr -ag gds "git diff --stat"
abbr -ag gdsc "git diff --stat --cached"
abbr -ag gdw "git diff --word-diff"
abbr -ag gdwc "git diff --word-diff --cached"
abbr -ag gdtool "git difftool"

abbr -ag gignore "git update-index --agssume-unchanged"
abbr -ag gunignore "git update-index --no-agssume-unchanged"

abbr -ag gf "git fetch"
abbr -ag gfa "git fetch --agll --prune"

abbr -ag gl "git pull"
abbr -ag glr "git pull --rebase"

abbr -ag glg "git log --stat --max-count=10"
abbr -ag glgg "git log --graph --max-count=10"
abbr -ag glgga "git log --graph --decorate --agll"
abbr -ag glo "git log --oneline --decorate --color"
abbr -ag glog "git log --oneline --decorate --color --graph"
abbr -ag gloo "git log --pretty=format:'%C(yellow)%h %Cred%ad %Cblue%an%Cgreen%d %Creset%s' --date=short"

abbr -ag gm "git merge"
abbr -ag gmt "git mergetool --no-prompt"

abbr -ag gp "git push"
abbr -ag gp! "git push --force-with-lease"
abbr -ag gpv "git push --no-verify"
abbr -ag gpv! "git push --no-verify --force-with-lease"
abbr -ag gpa "parallel git push --agll -- (git remote)"
abbr -ag gpaf "parallel git push --agll --force -- (git remote)"

abbr -ag gr "git remote -vv"
abbr -ag gra "git remote add"
abbr -ag grmv "git remote rename"
abbr -ag grrm "git remote remove"
abbr -ag grset "git remote --set-url"
abbr -ag grup "git remote update"
abbr -ag grv "git remote -v"

abbr -ag grb "git rebase"
abbr -ag grba "git rebase --agbort"
abbr -ag grbc "git rebase --continue"
abbr -ag grbi "git rebase --interactive"

abbr -ag grev "git revert"

abbr -ag grst "git reset"
abbr -ag grsth "git reset --hard"
abbr -ag grstp "git reset --patch"

abbr -ag grm "git rm"
abbr -ag grmc "git rm --cached"

abbr -ag grs "git restore"
abbr -ag grss "git restore --source"

abbr -ag gsh "git show"

abbr -ag gss "git status -s"
abbr -ag gst "git status"

abbr -ag gsta "git stash"
abbr -ag gstd "git stash drop"
abbr -ag gstp "git stash pop"
abbr -ag gsts "git stash show --text"

abbr -ag gsu "git submodule update"
abbr -ag gsur "git submodule update --recursive"
abbr -ag gsuri "git submodule update --recursive --init"

abbr -ag gts "git tag -s"
abbr -ag gtv "git tag | sort -V"

abbr -ag gsw "git switch"
abbr -ag gswc "git switch --create"

abbr -ag gwch "git whatchanged -p --agbbrev-commit --pretty=medium"

abbr -ag gco "git checkout"
abbr -ag gcb "git checkout -b"
