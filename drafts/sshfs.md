---
date: 1900-01-01
---


sshfs hq:/net/kodiak /usr/local/mnt/kodiak -oauto_cache,reconnect,volname=kodiak
/sbin/mount_nfs -o vers=3 kodiak:/volumes/lake/shared/users/henryic/deployment-preload/preload ~/tmp
