---
title: "Why Your SaaS Emails Aren’t Being Delivered and How to Fix This Issue"
authors: [milica]
image: /img/email/email-cover.jpg
tags: [webdev, tips, emails]
---
import ReactPlayer from 'react-player'
import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';
import ImgWithCaption from './components/ImgWithCaption'

If you’ve just built your SaaS web app and deployed it on a production server, you might be running into email deliverability issues. Transactional or marketing emails might not be landing in your users' inboxes. Don’t panic! This is a pretty common problem, especially for apps that run on newly registered domains.

We have seen a lot of [Wasp](https://wasp-lang.dev/) users facing similar challenges, thinking their toolkit was to blame. In our Discord community, we regularly help users who’ve just launched their first app with Wasp, and we've seen this issue pop up frequently. The bad news: your users and their email servers pulled a Gandalf move on you. The good news: no worries, this is something you can fix!

![Gandalf saying you shall not pass](https://media2.giphy.com/media/WpD30tFjzosDn7amXq/giphy.gif?cid=7941fdc68jh5kd0zboo37lcp48knhd8bewokjgz7kc9juwov&ep=v1_gifs_search&rid=giphy.gif&ct=g)

## 1. **Build up your domain reputation**

A new domain is often flagged as suspicious by email providers, causing your emails to land in spam or not be delivered at all. Google’s filtering is really heavy, especially if you’re trying to reach people with business addresses (user@theircompany.com). Even the basic signup confirmations have a high chance of bouncing when you’re sending them from a freshly registered domain.

To improve your domain reputation, do the following:

- **Use a custom domain for your emails**: If you’re still using a generic email service like Gmail, switch to a custom domain. This makes you more trustworthy and looks more professional.
- **Warm-up your domain**: You need to do this before you start onboarding your users. Any type of sudden bursts of emails from a new domain can be flagged as spam. For example, if you decide to launch on Product Hunt, you’ll get a spike in signups which increases the amount of sent emails. It’s possible that this spike triggers the alarms, and people stop receiving your emails. [There are numerous tools out there](https://letmegooglethat.com/?q=Email+warmup+tools) that can help you with this process. Don’t skip this step, it’s mandatory.
- **Keep the sending volume consistent**: Regular email sending patterns are seen as trustworthy. Inconsistent or high-volume bursts from a new domain can trigger spam filters.

## 2. **Authenticate your domain**

Authentication adds a layer of security to your emails, proving to email providers that you’re a legitimate sender and not the next prince of spam from the land of Spamlia. Here are the key records you need to set up with your DNS provider:

- **SPF**: This allows email servers to verify that emails sent from your domain are really coming from you.
- **DKIM**: This attaches a digital signature to your emails that enables email servers to confirm the email wasn’t tampered with in transit.
- **DMARC**: this one helps you control how your email domain handles unauthenticated emails.

Read more about these records and how to add them to your DNS servers [here](https://www.cloudflare.com/en-gb/learning/email-security/dmarc-dkim-spf/). 

![A girl saying and you're going to fix it](https://media4.giphy.com/media/ZdrDsXfFz1ZRhZQpLZ/giphy.gif?cid=7941fdc6zjvsa5atns4qklo6odlg3zjir92fy34lvymyfmx1&ep=v1_gifs_search&rid=giphy.gif&ct=g)

## 3. **Use professional email sending tools**

Instead of sending emails directly from your own server, consider using a third-party email service that specializes in this area. Tools like **SendGrid** or **Mailgun** have built-in features to help ensure your emails make it to the inbox. Wasp helps you to [add them to your stack](https://wasp-lang.dev/docs/advanced/email) with minimal configuration needed on your end.

They monitor and improve your domain reputation, manage bounces, and handle email authentication out-of-the-box. We’d recommend you to offload sending emails to them, so that you can focus on the core aspects of your business.

## 4. **Monitor your deliverability**

It’s important to keep an eye on how your emails are performing. Look for key metrics like bounce rate, open rate, and spam complaints. Most email sending services provide insights into your email deliverability, allowing you to make adjustments before your reputation gets damaged.

- **Bounce rate**: High bounce rates suggest you’re sending to invalid or outdated email addresses. Regularly clean your list to avoid this. If more than 3% of your emails bounce, your domain can get blocked by your email provider.
- **Spam complaints**: High spam reports and complaints can also lead to email providers blocking your domain. If users are marking your emails as spam, reconsider the content and frequency of your emails. Also, please don’t buy email lists off of Internet, those will do you more harm than good.

![meme saying are you looking into buying email lists](/img/email/meme.jpg)

## 5. **Create high-quality content**

What you write matters too. Emails are poorly written are more likely to be marked as spam.

- **Use a clear subject line**: Avoid clickbait or overly promotional language. Keep your subject lines clear and aligned with the content of your email. If your subject line is off, your email can directly land in spam or in the promotional inbox.
- **Personalize your emails**: Address your users by their name and offer content that is relevant to their needs. Personalized emails tend to have higher open and click-through rates.
- **Avoid spammy-looking keywords**: Words like “FREE,” “LIMITED OFFER,” and excessive use of exclamation marks can trigger spam filters. Keep your language professional and to the point. DON’T USE CAPS LOCK EVERYWHERE!

## Inbox access granted

![you can do this gif](https://media1.giphy.com/media/ACJuukdjBl65FwUFzT/giphy.gif?cid=7941fdc6u3cbcux0bo255tvvoxfhq9cuep0g28vcdk1cbryt&ep=v1_gifs_search&rid=giphy.gif&ct=g)

We know that email delivery issues are frustrating, but they are solvable. Start small - implement one or two changes. First, authenticate your domain and then [set up professional email sending tools](https://wasp-lang.dev/docs/advanced/email#using-the-mailgun-provider), Wasp supports some out of the box. 

You can monitor the performance over time, and improve your approach with every batch of emails. It’s not about getting everything perfect from the start, but about making the right decisions before you start onboarding your users.