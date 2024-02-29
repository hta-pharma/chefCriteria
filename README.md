
<!-- Insert badges here -->
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Library of criteria functions for chef-style AMNOG analyses

The chefCriteria package aims to provide a library of inclusion criteria
functions for use in AMNOG analyses created by the chef package.

As the functions found in chefCriteria are designed to be used with
chef, it may be unwieldy to use these functions independently.

# Developer Setup

## Install githooks

This project supports two styles of githooks.

1.  The first style is to use the githooks provided in the `.githooks`
    directory. To use these hooks, run the following command in the root
    of the project:

- These hooks are very simple just blocking the commit to protected
  branches. \`\` git config –local core.hooksPath .githooks/

<!-- -->


    2. The second is to install the precommit tool (for linux) [precommit](https://pre-commit.com/). 
      - These are much more powerful and can be used to run checks on the code before it is committed.

pipx install pre-commit \# use pipx to avoid polluting your global
python environment \# pip install pre-commit \# if you don’t want to use
pipx

# Then run in the root of repo:

pre-commit install \# Will take 2-3 minutes to run \#pre-commit run –all
\#Optionally run all checks over all files \`\`\`
