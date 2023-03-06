# org-ai

Minor mode for Emacs org-mode that provides access to OpenAI API's.

Inside an org-mode buffer you can use ChatGPT to generate text or DALL-E to generate images.

Note: In order to use this you'll need an [OpenAI account](https://platform.openai.com/)
and you need to get an API token. As far as I can tell, the current usage limits
for the free tier get you pretty far.

## Usage

Simply create an `ai` block and press `C-c C-c`:

### ChatGPT in org-mode

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

![chat-gpt in org-mode](doc/org-ai-demo-1.gif)

You can continue to type and press `C-c C-c` to create a conversation.


### DALL-E in org-mode

```org
#+begin_ai :image :size 256x256
Hyper realistic sci-fi rendering of super complicated technical machine.
#+end_ai
```

![dall-e in org-mode](doc/org-ai-demo-2.gif)

## Options

The `#+begin_ai...#+end_ai` block can take the following options:
- `:max-tokens number` - number of maximum tokens to generate (default: 120)
- `:temperature number` - temperature of the model (default: 1)
- `:top-p number` - top_p of the model (default: 1)
- `:frequency-penalty number` - frequency penalty of the model (default: 0)
- `:presence-penalty` - presence penalty of the model (default: 0)
- `:image` - generate an image instead of text
- `:size` - size of the image to generate (default: 256x256, can be 512x512 or 1024x1024)
- `:completion` - instead of using the chatgpt model, use the completion model
- `:model` - which model to use, see https://platform.openai.com/docs/models for a list of models

## Setup

### Melpa

The PR for [melpa is currently pending](https://github.com/melpa/melpa/pull/8429)

### Straight.el

```elisp
(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"
          :local-repo "org-ai"
          :files ("*.el" "README.md")))
```

### Manual

Checkout this repository.

```sh
git clone
https://github.com/rksm/org-ai
```

Then, if you use `use-package`:

```elisp
(use-package org-ai
  :load-path (lambda () "path/to/org-ai")
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit))
```

or just with `require`:

```elisp
(add-to-list 'load-path "path/to/org-ai")
(require 'org)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(setq org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")
(advice-add 'keyboard-quit :before #'org-ai-keyboard-quit))
```
