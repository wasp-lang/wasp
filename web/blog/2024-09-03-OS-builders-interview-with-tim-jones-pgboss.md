---
title: 'The Faces Behind Open Source Projects: Tim Jones and pg-boss'
authors: [milica]
image: /img/os-interviews/pg-boss-banner.jpeg
tags: [webdev, postgresql, database, os-maintainers]
---

![wasps interviewing Tim](/img/os-interviews/pg-boss-banner.jpeg)

We’re launching a new series of posts where we'll sit down with the folks who help us run our projects without expecting anyting in return. Yes, we're talking about open-source maintainers and builders, the people who dedicate their free time to make tech better. This is our way to say "Thank you!" to all of those who help us build and improve [Wasp](https://wasp-lang.dev/), as well as shape the webdev ecosystem.

In our first post, we had the chance to chat with Tim, the maintainer of [pg-boss](https://github.com/timgit/pg-boss), a library that makes managing job queues in PostgreSQL a breeze. We talked about what it’s like to maintain an open-source project, the ups and downs, and why Tim keeps coming back to make pg-boss better. If you’ve ever relied on open-source tools, this series is for you.

Let's dive in!

- **Please tell us a little bit about yourself. When and how did you get introduced to coding?**

I was first introduced to coding in grade school with Logo in the 80s. I built my first website on GeoCities in 97, then started coding professionally with Visual Basic in MS Access during the Y2K craze. I built my first multi-tenant SaaS app (an extranet) in 2002 before it was cool using Classic ASP. I spent at least a decade in C# coding web apps most of the time before finally settling down in the warm embrace of full stack JavaScript with Node.js for the last 10 years.

- **Could you introduce us to pg-boss? Was there a reason or use case you had for creating it?**

I joined a startup in 2015 and we needed an open source relational database so Mongo would stop ruining our lives. I had heard good things about Postgres, so I was excited to give it a chance. Our product also included Redis, but only for the purpose of hosting a job queue via the Kue package. This seemed excessive to manage another piece of infrastructure for a queue, especially for our low-volume requirements. I hadn’t used Redis before, so I started researching it.

I learned that Redis is a great choice when you need a fast, in-memory database. However, a job queue is a different use case, where you are guaranteeing to someone: “I promise to do this later”. If your Redis server crashes, you could lose a lot of jobs with the default configuration. Their persistence documentation even states, “if you want a degree of data safety comparable to what PostgreSQL can provide you”, you should use both RDB and AOF configurations. Within AOF, there is an option, “appendfsync always”, but it’s generally discouraged with warnings of poor performance. The recommended configuration is almost always “everysec”, but with the disclaimer, “you may lose 1 second of data if there is a disaster”. These warnings about safety should cause anyone in technical leadership to pause and ask some questions.

Even now in 2024 Redis seems to be the dominant queue persistence database in OSS, but it wasn’t designed to match the use case of a guaranteed job delivery system. Seeing the growth of open source Postgres-backed queues makes me optimistic that over time more development teams realize this mismatch and we’ll see a decline in popularity of the Redis queue projects. If your product relies on Redis for its queue, I encourage you to review your disaster recovery plans and server configurations. It’s concerning that the popularity of the “just use Redis for your queue” is likely producing a large population of applications vulnerable to losing jobs.

In 2015, the number 1 hit on Google for “Postgres job queue” was Brandur Leach’s “Postgres Job Queues & Failure By MVCC” blog post. LOL! The Internet replied with “don’t do this”. However, at this same point in time Postgres 9.5 was in beta and about to be released, and lo and behold a new core feature was added called SKIP LOCKED. This looked like a perfect fit, but it was too new for any packages to have this approach. After a successful prototype, I thought it seemed like a good opportunity to try creating the package myself, since I was new to Node and wanted to learn more about it. I started building it on nights and weekends, shipped version 0.0.1 in early 2016 then finally released 1.0 a year later.

- **How do you manage contributions and feature requests?**

The best thing about OSS in my opinion is in its name: “open”. Having the entire world of developers not only use your software, but also be able to read its code and make contributions produces the highest quality code. You will get questions you never thought about, and find bugs you didn’t know existed. In the case of bug fixes, most of the time it’s as simple as making sure said bug has a new unit test then merging a pull request (PR).

Managing feature requests is not easy, however. If you don’t accept any contributions, it seems to violate the principle of OSS. If you accept every contribution, you risk the project drifting away from its original design or becoming too complicated. The balance between these extremes is evaluating each feature request against what seems to be best aligned with the core purpose of the project. This becomes a non-technical decision sometimes, and may result in a “let’s agree to disagree” outcome. My desire is to try and merge all PRs that arrive, and I really appreciate all the contributors that have sent them. It’s not fun saying “no”, because someone put in the coding effort and they felt strongly enough about it to spend their free time on your project.

- **What are the unexpected challenges of managing a successful open-source project?**

I did not expect how time-consuming managing an open source project would be. This is probably the primary reason projects become abandoned over time if they are supported by only 1 or a few developers. For example, you could have a perfectly functioning code base in Node 0.12, then decide to upgrade to async/await later and have to rewrite everything. The same applies to changing a test suite, assertion package, or even striving to attain 100% code coverage.

- **Using Postgres as a queue solution has become quite popular recently :) What are your thoughts on differences between pg-boss and e.g. pgmq?**

This is encouraging to see. I learned about pgmq from this question. It looks like a great option for a pure SQL implementation as a Postgres extension. I haven’t used it so I won’t be able to offer a detailed comparison, but after a quick review I see it uses SKIP LOCKED and a partitioned job table, which are good baseline scalability requirements. Postgres extension version management is challenging, however, and usually involves database service restarts and downtime.

- **What is the largest scale you’ve seen or heard pg-boss used at? How scalable / how far can you go with running background jobs directly in pg?**

In terms of job storage, we’ve been able to store 2-3 million jobs in v9’s shared job storage and survive, but not really thrive, as database performance starts to degrade. Before v10, in order to mitigate this you have to use configuration settings to move completed jobs into the archive more often.

Once record counts are under control, in terms of job throughput, the number of concurrent queries to Postgres can be very high, especially when using connection poolers and job batching. I have a speed test in the suite that can both fetch and then complete 10,000 jobs in 0.5 seconds, a metric you would not easily be able to achieve even in some dedicated queuing products. For job creation on the other hand, you have the full power of SQL available in Postgres via INSERT or even COPY.

:::tip[Fun fact!]
Did you know that Wasp uses pg-boss under the hood? We built Wasp Jobs on top of pg-boss, and [here's how we did it](https://wasp-lang.dev/blog/2022/06/15/jobs-feature-announcement).
:::

- **While pg-boss has many production use cases, is there a scenario or limit where teams may need a traditional queue instead?**

Because of the limitations in v9, we use pg-boss and AWS SQS side by side. Some of our queues rely heavily on pg-boss’s rate limiting and uniqueness features. We use SQS for queues that can grow very large quickly. Now that we’ve upgraded to v10, we might consider switching back for cost reasons, since SQS is not cheap at this scale.

- **There is a new version of [pg-boss v10](https://github.com/timgit/pg-boss/releases/tag/10.0.0), what should people be excited about?**

pg-boss v9 and below uses a shared job table across all queues. Once you run into the storage limitations mentioned earlier, all queues are affected. This is especially problematic since queues were also used internally for maintenance and scheduling. Maintenance controls archival, so if this queue can’t be processed, it prevents pg-boss from auto-recovering from this via the retention policy.

The design goal in v10 was to mitigate this performance issue first by isolating each queue into a dedicated table via partitioning. If a queue were to become backlogged with millions of jobs, it would affect the performance of that queue only. Then, if needed, you could use the newly added deleteJob() function in your workers, inspired by AWS SQS, to keep the record count as low as possible.

Another very useful new feature is queue policies. Over the years there were several issues opened around worker concurrency, which added several functions and configuration which made the API more complicated and difficult to understand. For example, there was a function sendSingleton(), which behaved entirely different from a configuration option named enforceSingletonQueueActiveLimit. In v10, these were all removed in favor of queue policies. A couple of examples are “short” queues, which only allow 1 pending job, no matter how many jobs are submitted, and “singleton” queues, which allow only 1 job to be active.

There are several other enhancements made to improve quality of life in v10:

- All jobs have 2 retries enabled by default
- FIPS compliant (dropped internal usage of MD5 hashing)
- Replication support for HA or read-replicas (every table now has a primary key)
- Serverless function supervision (maintain() function that can be run from another scheduler)
- Postgres dependency-free (no pgcrypto extension needed uuid generation)
- Dead letter queues (Replaces “completion job” feature and gains retry support for processing them)

- **What are your thoughts on things like pg_render [which is a way to render HTML in Postgres](https://www.madewithsupabase.com/p/pg-render)? [](https://www.madewithsupabase.com/p/pg-render)Is that going too far?**

Since I’m a Postgres fan, it’s hard for me to see how adding more capabilities to it would be considered “going too far”. This particular project appears to be a simple abstraction of other open source packages and could be a useful case study of creating a Postgres extension in Rust. Since this is compiled, it should even have better performance benchmarks over other packages with the same feature in non-compiled languages.

Overall, I agree with the argument for simplicity in Stephan Schmidt’s article “Just Use Postgres for Everything”. The only issue with his article, which he continues to edit after all these years, is Stephan recommends River, a new OSS queue using SKIP LOCKED in Go, when we could have just mentioned little ole pg-boss that’s been around for years. The irony is that River was built by Brandur “don’t use Postgres queues” Leach. I’m kidding around here, of course, but who doesn’t enjoy a bit of irony?

- **What advice would you give to developers or companies who are thinking of open sourcing their projects?**

For developers, I would encourage starting off by making a contribution to a package you use daily. For example, if you’ve ever thought “I like this package, but I think it could be better if it had this feature”, that might be something you could add. If you have an idea of a new project, my advice would be to focus on the MVP (minimally viable product). The more features you add, the more time required to release it and the more maintenance required to keep it running. This is the primary reason I don’t include a web API or UI with pg-boss, even though I’m sure it would make it more popular.

For companies, I would point to successful startups that became popular and attracted funding because of their OSS projects. TimescaleDB, Supabase, and Citus are examples just from the Postgres ecosystem.

- **What is the main reason you keep working on pg-boss? What do you get out of it?**

It’s nice to be able to change what type of development you do occasionally. It gives you a different perspective and usually gives you new insights into your primary job. When you have responsibility over a popular package, it also adds some motivation to make sure it remains healthy. It’s not a good feeling when your package causes bad performance on a database server because of its own success (a large queue backlog could mean your company is growing). I did sign up for GitHub Sponsors a couple years ago, and I appreciate every company and developer that has benefited from pg-boss making contributions, but it’s still far from being able to fund itself.

- **Have you ever thought of introducing a paid product on top of pg-boss? Why yes/no?**

I haven’t spent time evaluating what that would look like, but it is an intriguing idea for sure. If I was able to work on pg-boss full-time, there are several potential new concepts that could be explored now that partitioning and queue policies have been created.

For example, someone recently opened an issue to request arbitrary JSON querying capability to be included in fetch(). If done globally across all queues, this would require indexing the jsonb payload, which in some cases could be large, slowing down reads and writes. However, if applied only to a single partition, this index could be isolated to only the queue that needs it via a new policy. Postgres allows different indexes and unique constraints between tables within the same partitioning hierarchy, which could be used for this policy.

Furthermore, very large partitions could even have subpartitions, creating a tree-like structure for scalability purposes. This strategy has already been proven at scale in Postgres by the TimescaleDB team, and could potentially result in removing its storage capacity challenges entirely.

Yet another area of exploration is encapsulating all features into Postgres functions to make the logic of pg-boss more easily integrated with non-Node.js platforms, as most of the logic in pg-boss is actually already pure SQL. Some have also mentioned to me that it would be nice if we had a standard queue data schema that could be shared across platforms, which sounds like a noble goal, but challenging since it would involve collaboration across OSS projects to complete.

- **Apart from yours, is there an open source tool or project you’re particularly excited about?**

I’m a big fan of [Supabase](https://supabase.com/) at the moment. I’m currently evaluating their use of JWT claims and Postgres RLS for SaaS multi-tenancy. They have open sourced several of their projects as well, such as supavisor, a connection pooler. Their product also happens to use pg-boss internally, which means… “they use pg-boss btw” 🙂