# org-ai [![MELPA](https://melpa.org/packages/org-ai-badge.svg)](https://melpa.org/#/org-ai)

Minor mode for Emacs org-mode that provides access to OpenAI API's. Inside an org-mode buffer you can
- use ChatGPT to generate text, having full control over system and user prompts ([demo](#chatgpt-in-org-mode))
- Speech input and output! Talk with your AI!
- generate images and image variations with a text prompt using DALL-E ([demo 1](#dall-e-in-org-mode), [demo 2](#image-variations))

__NOTE 1:__ v0.2.0 comes with a big update:

#### Speech input/output. Talk with your AI!
- In org-mode / `#+begin_ai..#+end_ai` blocks:
  - `C-c r` to record and transcribe speech via whisper.el in org blocks.
- Everywhere else:
    - Enable speech input with `org-ai-talk-input-toggle` for other commands (see below).
- Enable speech output with `org-ai-talk-output-enable`. Speech output uses os internal speech synth (macOS) or `espeak` otherwise.
- See [Setting up speech input / output](#setting-up-speech-input--output) below for more details.

#### Non-org-mode commands
- `org-ai-prompt`: prompt the user for a text and then print the AI's response in current buffer.
- `org-ai-on-region`: Ask a question about the selected text or tell the AI to do something with it.
- `org-ai-summarize`: Summarize the selected text.
- `org-ai-explain-code`: Explain the selected code.
- `org-ai-refactor-code`: Tell the AI how to change the selected code, a diff buffer will appear with the changes.

#### Other improvements
- In org-mode / `#+begin_ai..#+end_ai` blocks:
    - Press `C-c <backspace>` (`org-ai-kill-region-at-point`) to remove the chat part under point.
    - `org-ai-mark-region-at-point` will mark the region at point.
    - `org-ai-mark-last-region` will mark the last chat part.

#### Other commands
- `org-ai-open-account-usage-page` show how much money you burned.
- `org-ai-install-yasnippets` install snippets for `#+begin_ai..#+end_ai` blocks.
- `org-ai-open-request-buffer` for debugging, open the request buffer.


_Note2: In order to use this you'll need an [OpenAI account](https://platform.openai.com/) and you need to get an API token. As far as I can tell, the current usage limits for the free tier get you pretty far._


------------------------------


## Table of Contents

- [Setup](#setup)
    - [Melpa](#melpa)
    - [Straight.el](#straightel)
    - [Manual](#manual)
    - [Setting up speech input / output](#setting-up-speech-input--output)
- [Features](#features)
- [Demos](#demos)
    - [ChatGPT in org-mode](#chatgpt-in-org-mode)
    - [DALL-E in org-mode](#dall-e-in-org-mode)
    - [Image variations](#image-variations)
- [Options](#options)
- [FAQ](#faq)


## Setup

My personal config for org-ai can be found in [this gist](https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34).

### Melpa

org-ai is on Melpa: https://melpa.org/#/org-ai. If you have added Melpa to your package archives with

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
```

you can install it with:

```elisp
(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  (setq org-ai-default-chat-model "gpt-4")
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))
```

### Straight.el

```elisp
(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"
          :local-repo "org-ai"
          :files ("*.el" "README.md" "snippets")))
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
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets))
```

or just with `require`:

```elisp
(add-to-list 'load-path "path/to/org-ai")
(require 'org)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-install-yasnippets) ;; if you are using yasnippet and want `ai` snippets
(setq org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")
```

### API key with auth-source

`org-ai` supports `auth-source` for retrieving your API key. You can store a secret in the format

```
machine api.openai.com login org-ai password <your-api-key>
```

If this is present, `org-ai-openai-api-token` will be automatically set that value. If you do not want `org-ai` to try to retrieve the key from `auth-source`, you can set `org-ai-use-auth-source` to `nil` before loading `org-ai`.

### Setting up speech input / output

This has been tested on macOS and Linux. Someone with a Windows computer, please test this and let me know what needs to be done to make it work (Thank You!).

The speech input uses [whisper.el](https://github.com/natrys/whisper.el) and `ffmpeg`. You need to clone the repo directly or use [straight.el](https://github.com/radian-software/straight.el) to install it.

1. install ffmpeg (e.g. `brew install ffmpeg` on macOS) or `sudo apt install ffmpeg` on Linux.
2. Clone whisper.el: `git clone https://github.com/natrys/whisper.el path/to/whisper.el`

You should now be able to load it inside Emacs:

```elisp
(use-package whisper
  :load-path "path/to/whisper.el"
  :bind ("M-s-r" . whisper-run))
```

Now also load `org-ai-talk`, it will not be loaded automatically:

```elisp
(use-package greader :ensure)
(require 'whisper)
(require 'org-ai-talk)

;; macOS speech settings, optional
(setq org-ai-talk-say-words-per-minute 210)
(setq org-ai-talk-say-voice "Karen")
```

#### macOS specific steps

On macOS you will need to do two more things:
1. Allow Emacs to record audio
2. Tell whisper.el what microphone to use

##### 1. Allow Emacs to record audio
You can use the [tccutil helper](https://github.com/DocSystem/tccutil):

```sh
git clone https://github.com/DocSystem/tccutil
cd tccutil
sudo python ./tccutil.py -p /Applications/Emacs.app -e --microphone
```

When you now run `ffmpeg -f avfoundation -i :0 output.mp3` from within an Emacs shell, there should be no `abort trap: 6` error.

##### 2. Tell whisper.el what microphone to use

You can use the output of `ffmpeg -f avfoundation -list_devices true -i ""` to list the audio input devices and then tell whisper.el about it: `(setq whisper--ffmpeg-input-device ":0")`. `:0` is the microphone index, see the output of the command above to use another one.

I've created an emacs helper that let's you select the microphone interactively. See [this gist](https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34#file-init-org-ai-el-L6).

My full speech enabled config then looks like:

```elisp
(use-package whisper
  :load-path (lambda () (expand-file-name "lisp/other-libs/whisper.el" user-emacs-directory))
  :config
  (setq whisper-model "base"
        whisper-language "en"
        whisper-translate nil)
  (when *is-a-mac*
    (rk/select-default-audio-device "Macbook Pro Microphone")
    (when rk/default-audio-device)
    (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device))))
```


## Features

### `#+begin_ai...#+end_ai` special blocks

Similar to org-babel, these blocks demarcate input (and for ChatGPT also output) for the AI model. You can use it for AI chat, text completion and text -> image generation. See [options](#options) below for more information.

Create a block like

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

and press `C-c C-c`. The Chat input will appear inline and once the response is complete, you can enter your reply and so on. See [the demo](#chatgpt-in-org-mode) below. You can press `C-g` while the ai request is running to cancel it.

You can also modify the _system_ prompt and other parameters used. The system prompt is injected before the user's input and "primes" the model to answer in a certain style. For example you can do:

```org
#+begin_ai :max-tokens 250
[SYS]: Act as if you are a powerful medival king.
[ME]: What will you eat today?
#+end_ai
```

This will result in an API payload like

```json
{
  "messages": [
    {
      "role": "system",
      "content": "Act as if you are a powerful medival king."
    },
    {
      "role": "user",
      "content": "What will you eat today?"
    }
  ],
  "model": "gpt-3.5-turbo",
  "stream": true,
  "max_tokens": 250,
  "temperature": 1.2
}
```

For some prompt ideas see for example [Awesome ChatGPT Prompts](https://github.com/f/awesome-chatgpt-prompts).

When generating images using the `:image` flag, images will appear underneath the ai block inline. Images will be stored (together with their prompt) inside `org-ai-image-directory` which defaults to `~/org/org-ai-images/`.

### Image variation

You can also use an existing image as input to generate more similar looking images. The `org-ai-image-variation` command will prompt for a file path to an image, a size and a count and will then generate as many images and insert links to them inside the current `org-mode` buffer. Images will be stored inside `org-ai-image-directory`. See the [demo](#image-variations) below.

[For more information see the OpenAI documentation](https://platform.openai.com/docs/guides/images/variations). The input image needs to be square and its size needs to be less than 4MB. And you currently need curl available as a command line tool[^1].

[^1]: __Note:__ Currenly the image variation implementation requires a command line curl to be installed. Reason for that is that the OpenAI API expects multipart/form-data requests and the emacs built-in `url-retrieve` does not support that (At least I haven't figured out how). Switching to `request.el` might be a better alternative. If you're interested in contributing, PRs are very welcome!


## Demos

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

![dall-e image generation in org-mode](doc/org-ai-demo-3.gif)


## Options

The `#+begin_ai...#+end_ai` block can take the following options.

### For ChatGPT
By default, the content of ai blocks are interpreted as messages for ChatGPT. Text following `[ME]:` is associated with the user, text following `[AI]:` is associated as the model's response. Optionally you can start the block with a `[SYS]: <behahvior>` input to prime the model (see `org-ai-default-chat-system-prompt` below).

- `:max-tokens number` - number of maximum tokens to generate (default: nil, use OpenAI's default)
- `:temperature number` - temperature of the model (default: 1)
- `:top-p number` - top_p of the model (default: 1)
- `:frequency-penalty number` - frequency penalty of the model (default: 0)
- `:presence-penalty` - presence penalty of the model (default: 0)
- `:sys-everywhere` - repeat the system prompt for every user message (default: nil)

The following custom variables can be used to configure the chat:

- `org-ai-default-chat-model` (default: `"gpt-3.5-turbo"`)
- `org-ai-default-max-tokens` How long the response should be. Currently cannot exceed 4096. If this value is too small an answer might be cut off (default: nil)
- `org-ai-default-chat-system-prompt` How to "prime" the model. This is a prompt that is injected before the user's input. (default: `"You are a helpful assistant inside Emacs."`)
- `org-ai-default-inject-sys-prompt-for-all-messages` Wether to repeat the system prompt for every user message. Sometimes the model "forgets" how it was primed. This can help remind it. (default: `nil`)

### For DALL-E

When you add an `:image` option to the ai block, the prompt will be used for image generation.

- `:image` - generate an image instead of text
- `:size` - size of the image to generate (default: 256x256, can be 512x512 or 1024x1024)
- `:n` - the number of images to generate (default: 1)

The following custom variables can be used to configure the image generation:
- `org-ai-image-directory` - where to store the generated images (default: `~/org/org-ai-images/`)

### Other text models

The older completion models can also be prompted by adding the `:completion` option to the ai block.

- `:completion` - instead of using the chatgpt model, use the completion model
- `:model` - which model to use, see https://platform.openai.com/docs/models for a list of models

For the detailed meaning of those parameters see the [OpenAI API documentation](https://platform.openai.com/docs/api-reference/chat).

The following custom variables can be used to configure the text generation:

- `org-ai-default-completion-model` (default: `"text-davinci-003"`)

## FAQ

### Is this OpenAI specfic?

Currently yes but once there are more high-quality APIs available I'm planning on supporting those as well.
