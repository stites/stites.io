---
date: 1900-01-01
---


find . -name ac5ab5ad-d9fa-46fe-86fe-106dd2d99261 2>&1 | grep -v 'Permission denied'

moves all stderr (2) to stdout (1), then greps for a specific error 'Permission denied' and reverses what is grepped with `-v`
