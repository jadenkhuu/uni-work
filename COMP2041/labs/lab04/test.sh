#!/bin/dash

echo -n "Do you like learning Shell? "
read answer
# get first letter of answer connverted to lower case
answer="$(
  echo "$answer"|
  cut -c1|
  tr A-Z a-z )"

if test "$answer" = "y"
then
  response=":)"
elif test "$answer" = "n"
then
  response=":("
else
  response="??"
fi