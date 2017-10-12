---
title: Switching to Kinesis Advantage
author: joomy
tags: keyboard, mac, vim
description: My new keyboard and related workflow changes
---

I've been looking for a keyboard that would allow me to type without moving my
wrists and I ended up acquiring a used Kinesis Advantage from eBay.
This post will describe how I adapted my workflow to my new keyboard.

<a href="http://i.imgur.com/FYexM3t.jpg"><img src="http://i.imgur.com/FYexM3tl.jpg" /></a>

My thumb cluster keycaps are not accurately named, so:

* Run and Patrn keys are Command.
* Near is Option.
* Far is Control.
* Space/0 is spacebar.
* Black big key on the left is Escape.
* Dark gray big key on the right is Enter.

On left and right edges of the keyboard, Step keys are Shift, Mode is where
CapsLock is supposed to be, and the key that has "Function" is Tab.

## Workflow

I use a [Thinkpad x220t that runs OS X El Capitan](http://x220.mcdonnelltech.com/).
There is a very limited number of applications that I daily use, namely
Firefox, Vim (in iTerm2), Preview and Spotify.

Let's start with my key remaps. (compared to the standard Advantage bindings)

* I switched the Advantage to [Mac mode](https://www.kinesis-ergo.com/support/technical-support/faqs-advantage-keyboard/),
  which changes the Command, Option and Control keys to the setting described above.
* Swapping space and backspace: I am used to pressing space with my left thumb
  and backspace with my right hand. There isn't much point in swapping them after
  this point, so I moved space to the left and backspace to the right.
* Replacing delete with escape: I never use delete. But I use Vim all the time
  so I need a convenient escape key. I remapped the second big key (the black one) in my left
  thumb cluster to escape.
* Swapping up and down: By default Up is to the left of Down, which is
  unintuitive for me because I'm used to `j` and `k` in Vim, where `j` is down
  and `k` is up.

Now the shortcuts that are not application specific. There are
three major Mac shortcuts that I use all the time:

* Command + Tab: (switches to a different app) This shortcut is not too hard, I use my left thumb and left
  pinky without compromising much from other fingers' positioning on the home
  row. I still created a macro in the keyboard that maps CapsLock to Command + Tab
  so that if I quickly need to go to the previous app, that is only one convenient key.
* Command + backtick (`): (switches to a different window of the same app)
  Given the unusual position of the backtick, this one is not convenient.
  To be able to appease both ANSI and ISO keyboard users, Advantage has two
  backslash keys. For ANSI users it's in the right key
  well, and for ISO users it's in the left key well,
  where my Hyper key is located. I mapped that unused key to Command +
  backtick. The hyper keycap is [not supposed to mean
  anything](https://askubuntu.com/questions/19558/what-are-the-meta-super-and-hyper-keys).
* Command + Space: (opens Spotlight, i.e. search bar) This is fine. I can use
  my thumb to hit both at the same time with minimal movement.

![Command + Space](http://i.imgur.com/9fEzTA7m.jpg)

### Firefox

Heeding [Ayberk](https://github.com/ayberkt/)'s advice, I have been using
[VimFx](https://addons.mozilla.org/en-US/firefox/addon/vimfx/) to browse the
web, which helped me reduce my mouse usage tremendously. This was also a necessary
step to stabilize my wrists most of the time.

Even with default VimFx settings, there were some browser actions that were
uncomfortable on the Advantage. Default tab switch commands on Firefox are
Command+Option+left (or right), which requires a weird right hand movement on
and Advantage. Right thumb presses Command and Option, and some other right
hand finger pressed an arrow.

![Tab switch](http://i.imgur.com/26tefoXm.jpg)

Surely one can remap the keyboard and move the left and right arrow keys to the
left hand, but I

* didn't want to lose the muscle memory I have from using the arrow keys and HJKL.
* shouldn't have to move my hands and fingers drastically for an action like
switching to another tab that I perform numerous times every day. Normally my
thumb rests between enter and backspace and I would have to move it up every time.

![Thumb cluster](http://i.imgur.com/NKAqcH3l.jpg)

So I looked for alternative solutions. What I realized is that VimFx provides a
way for me to perform the same actions Home/End/PageUp/PageDown keys do.
This means I can remap those keys to something else. My solution was to use the
bottom ones to go to the previous/next tab, and the upper ones to move the
current tab to the left or right. The primary reason for this is that I don't
move the tabs around as much as I switch to other tabs, and the bottom keys are
easier to reach than the upper ones.

![VimFx settings](http://i.imgur.com/oVZJuOb.png)

Surely this doesn't correspond well to the actual functions of these keys, but
it makes the most sense to me in terms of how much I need to move my hand to
switch tabs. Most important of all, I don't need to move my wrist.

### iTerm

My habit of using tabs manifests itself in iTerm as well. I assigned End and
PageDown to switch to the previous/next tab. However, I also use splits all the
time, so I assigned Home and PageUp to switch to the previous/next split. (or
    in iTerm terminology, pane)

### Vim

As if that's not enough, I use tabs and split in Vim as well. I already have
Vim shortcuts for them, most importantly `gt` and `gT` for switching Vim tabs.
(You probably noticed that I had them in VimFx as well.) The problem with that
is that they require too many keystrokes that are inconvenient to type
consequently. The g and t keys are exactly above each other because of
ortholinearity, and unless I use my index and middle finger together, it's not
ideal, and I want to keep my fingers at the home row as much as possible.

![Trying to press g and t](http://i.imgur.com/5AKzHOzm.jpg)

So my solution was to use space and backspace to move to the previous/next tab.
In my settings they were not already assigned to anything.

The other problem is that I was using backslash as my leader key. Given that I
will sometimes have to use my laptop keyboard, I didn't want to switch to an
entirely different leader key. So I mapped my enter key to the leader key. I
also have pressing enter twice as a quick save shortcut.

```vim
" Kinesis Advantage
nmap <Space> gT
nmap <Backspace> gt
nmap <Enter> \
nmap \<Enter> :w<CR>
```

### Other apps

For Preview and Spotify, my options are more limited. I can still use the
Home/End/PageUp/PageDown keys as intended. I also don't mind using my mouse so much for Preview.

For Spotify, I set up my functions keys as media keys using [Karabiner](https://pqrs.org/osx/karabiner/).

![Karabiner settings](http://i.imgur.com/aT0INPn.png)

I don't remember a time I needed to use the function keys on Mac, but if I
absolutely need to I suppose I can disable Karabiner.

## Keycaps

I wasn't sure whether I should keep the stock keycaps that came with the
Kinesis Advantage, then I decided to change them because I already had a set of
[DSA Granite](https://www.massdrop.com/buy/granite-keycap-set)
around and after the macros and remappings I needed something that
makes more sense. Also I like the [flat profile](http://i.imgur.com/xdDfWRL.png) of the DSA keycaps.
I should note that [SA profile keys do not fit by
default](http://adereth.github.io/blog/2015/02/17/sa-profile-keys-on-a-kinesis-advantage/)
on an Advantage, but [DSA
does](http://sitr.us/2014/05/19/kinesis-advantage-with-dsa-keycaps.html).

Since the keycap sizes of the Kinesis Advantage are a bit unusual, I had to use
1x keycaps for the key on the side, which should in fact be 1.25x. I also
needed 4 keys that are of size 2x for the thumb clusters. I could find those
keys from a grab bag I have. (not to mention the 1x backslash key which is has
    a different color)

My other concern was that I wouldn't be able to use the thumb clusters as well
as they are supposed to be used. Especially to reach the command, control and
option keys, I would need to be careful not to press other keys in the cluster. To
avoid that, I used 4 SA profile keys (taller flat keycaps) from the grab bag.

![Left thumb cluster](http://i.imgur.com/P5Se1aum.jpg)
![Right thumb cluster](http://i.imgur.com/gjj5nPvm.jpg)

## Conclusion

It has been a week since I got my Kinesis Advantage, so far I found it nothing
but delightful!  [Ortholinearity](http://blog.roastpotatoes.co/review/2015/09/20/ortholinear-experience-atomic/)
took some time to get used to, I kept typing m instead of n for a couple days,
and realized that I was pressing y with my left hand before, but now I
have a chance of fixing my bad typing habits. My typing speed dropped in
the first days but I was able to reach the same speed I had before in 4-5
days. I don't think I'll switch to a different keyboard for a long time, though
I might consider [attaching some kind of a trackpad/touchpad module](https://www.petekeen.net/mounting-a-magic-trackpad-on-a-kinesis-advantage-keyboard) on it later.

-------

## Update

I acquired an [Ergo Touchpad](http://www.ergonomictouchpad.com/) and attached
on the Advantage with Velcro. I also changed the Home/End/PageUp/PageDown
keycaps with blank SA keycaps to make them easier to reach with my thumb.

![Updated](http://i.imgur.com/1PgmNngl.jpg)

Since Ergo Touchpad doesn't have a middle click, I added some extra settings to
Karabiner to have key bindings for mouse keys. Now my `private.xml` file for
Karabiner looks like this:

```xml
<?xml version="1.0"?>
<root>
  <item>
    <name>Simultaneous Key Presses [W+S] to LeftClick</name>
    <identifier>remap.simultaneouskeypresses_ws2leftclick</identifier>
    <autogen>__SimultaneousKeyPresses__ KeyCode::W, KeyCode::S, PointingButton::LEFT</autogen>
  </item>
  <item>
    <name>Simultaneous Key Presses [E+D] to MiddleClick</name>
    <identifier>remap.simultaneouskeypresses_ed2middleclick</identifier>
    <autogen>__SimultaneousKeyPresses__ KeyCode::E, KeyCode::D, PointingButton::MIDDLE</autogen>
  </item>
  <item>
    <name>Simultaneous Key Presses [R+F] to RightClick</name>
    <identifier>remap.simultaneouskeypresses_rf2rightclick</identifier>
    <autogen>__SimultaneousKeyPresses__ KeyCode::R, KeyCode::F, PointingButton::RIGHT</autogen>
  </item>
</root>
```

Now I can use the "mouse" with my left hand on the home row and my right hand on the touchpad.
