# org-ai

Minor mode for Emacs org-mode that provides access to OpenAI API's.

Inside an org-mode buffer you can use ChatGPT to generate text or DALL-E to generate images.

Simply create an `ai` block and press `C-c C-c`:

## ChatGPT in org-mode

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

![chat-gpt in org-mode](doc/org-ai-demo-1.gif)

You can continue to type and press `C-c C-c` to create a conversation.


## DALL-E in org-mode

![dall-e in org-mode](doc/org-ai-demo-2.gif)
