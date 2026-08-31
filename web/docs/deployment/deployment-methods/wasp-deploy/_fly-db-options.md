All database options are optional.

- `--db-vm-size <vmSize>` sets the database VM size. It defaults to `shared-cpu-1x`. The VM size determines the CPU kind, CPU count, and memory. Use the following options to override those values:
  - `--db-vm-memory <vmMemory>` sets the database VM memory in MB.
  - `--db-vm-cpus <vmCpus>` sets the database VM CPU count.
  - `--db-vm-cpu-kind <vmCpuKind>` sets the database VM CPU kind.
- `--db-initial-cluster-size <initialClusterSize>` sets the initial number of database machines. It defaults to `1`.
- `--db-volume-size <volumeSize>` sets the database volume size in GB. It defaults to `1`.
- `--db-image <dbImage>` sets the PostgreSQL Docker image. It defaults to `flyio/postgres-flex:18`.

Some apps need more database memory than Fly provides by default. If yours does, use `--db-vm-memory` when you create the database.

See Fly.io's [`postgres create` reference](https://fly.io/docs/flyctl/postgres-create/) for details and its [VM size documentation](https://fly.io/docs/flyctl/platform-vm-sizes/) for available machine sizes.

Fly accepts only certain combinations of CPU kind, CPU count, and memory. If you set `--db-vm-cpus` or `--db-vm-cpu-kind`, pick a `--db-vm-memory` value that fits Fly's [machine sizing rules](https://fly.io/docs/machines/guides-examples/machine-sizing/).
