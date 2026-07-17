const outputType = process.argv[2];
const output = outputType === "stderr" ? process.stderr : process.stdout;

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const writeSplitChars = async () => {
  for (let i = 0; i < 10; i++) {
    output.write(Buffer.from([0xe2]));
    await sleep(5);
    output.write(Buffer.from([0x82]));
    await sleep(5);
    output.write(Buffer.from([0xac]));
    await sleep(5);
  }
};

writeSplitChars().catch((err) => {
  process.stderr.write(String(err));
  process.exit(1);
});
