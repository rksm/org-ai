# org-ai

Minor mode for Emacs org-mode that provides access to OpenAI API's. Inside an org-mode buffer you can use ChatGPT to generate text or DALL-E to generate images. Implemented in pure Emacs Lisp, no external dependencies required (except for image variations[^1]).

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

You can continue to type and press `C-c C-c` to create a conversation. `C-g` will interrupt a running request.


### DALL-E in org-mode

```org
#+begin_ai :image :size 256x256
Hyper realistic sci-fi rendering of super complicated technical machine.
#+end_ai
```

![dall-e in org-mode](doc/org-ai-demo-2.gif)

### Image variations

You can create [image
variations](https://platform.openai.com/docs/guides/images/variations) of any
image that you have in form of a file. The image needs to be square and its size
needs to be less than 4MB. And you need curl[^1]. `M-x org-ai-image-variation`
or (`C-c M-a v`) will prompt you for an image file and then create new image
variation(s).

![dall-e image generation in org-mode](doc/org-ai-demo-3.gif)

[^1]: __Note:__ Currenly the image variation implementation requires a command line curl to be installed. Reason for that is that the OpenAI API expects multipart/form-data requests and the emacs built-in `url-retrieve` does not support that (At least I haven't figured out how). Switching to `request.el` might be a better alternative. If you're interested in contributing, PRs are very welcome!


## Options

The `#+begin_ai...#+end_ai` block can take the following options.

### For ChatGPT
By default, the content of ai blocks are interpreted as messages for ChatGPT. Text following `[ME]:` is associated with the user, text following `[AI]:` is associated as the model's response.

- `:max-tokens number` - number of maximum tokens to generate (default: 120)
- `:temperature number` - temperature of the model (default: 1)
- `:top-p number` - top_p of the model (default: 1)
- `:frequency-penalty number` - frequency penalty of the model (default: 0)
- `:presence-penalty` - presence penalty of the model (default: 0)

### For DALL-E

When you add an `:image` option to the ai block, the prompt will be used for image generation.

- `:image` - generate an image instead of text
- `:size` - size of the image to generate (default: 256x256, can be 512x512 or 1024x1024)
- `:n` - the number of images to generate (default: 1)

### Other text models

The older completion models can also be prompted by adding the `:completion` option to the ai block.

- `:completion` - instead of using the chatgpt model, use the completion model
- `:model` - which model to use, see https://platform.openai.com/docs/models for a list of models

For the detailed meaning of those parameters see the [OpenAI API documentation](https://platform.openai.com/docs/api-reference/chat).

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

## FAQ

### Is this OpenAI specfic?

Currently yes but once there are more high-quality APIs available I'm planning on supporting those as well.

