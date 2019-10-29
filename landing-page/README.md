
Currently we are somewhat in a mess here, since we are mixing generated files and source files.

Ideally we should define simple build process that will create 'dist/', which we commit and then push to gh-pages.
Even better would be to not commit 'dist/', but for that we need to use smth else than gh-pages.

Right now, this is how it works:
 - Make changes to the page (don't edit main.css, edit sass instead).
 - Run `npm build-sass` to build css files.
 - Commit (to master, or to other branch for review before getting it into master).
 - Once you get it to master, run `./deploy-master-to-gh-pages.sh` to deploy the changes.