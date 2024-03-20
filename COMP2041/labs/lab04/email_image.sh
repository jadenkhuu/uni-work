#!/bin/dash

for arg in $@
do
  display $arg

  echo -n "Address to e-mail this image to? "
  read email

  if test "$email" = ''
  then
    echo "No email sent"
    continue
  fi

  echo -n "Message to accompany image? "
  read message

  echo "$message" | mutt -s 'image' -e 'set copy=no' -a $arg -- $email
  echo "$arg sent to $email"

done