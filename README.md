# org-ai

Minor mode for Emacs org-mode that provides access to OpenAI API's.

Inside an org-mode buffer you can use ChatGPT to generate text or DALL-E to generate images.

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


## Setup

Checkout this repository.

```elisp
(use-package org-ai
  :load-path (lambda () (expand-file-name "lisp/other-libs/org-ai" user-emacs-directory))
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>"")
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  (advice-add 'keyboard-quit :before #'org-ai-keyboard-quit))
```
