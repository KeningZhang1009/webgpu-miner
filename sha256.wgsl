// sha256.wgsl
const SHA256_BLOCK_SIZE: u32 = 64;  // 64字节/块
const K: array<u32, 64> = array<u32, 64>(
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1,
  0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
  0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786,
  0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147,
  0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
  0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
  0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a,
  0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
  0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
);

struct Block {
  data: array<u32, 16>  // 64字节块（16个u32）
};

struct State {
  state: array<u32, 8>   // 哈希中间状态
};

@group(0) @binding(0) var<storage, read>  inputBlocks: array<Block>;
@group(0) @binding(1) var<storage, read_write> outputHashes: array<array<u32, 32>>;
@group(0) @binding(2) var<storage, read_write> chainStates: array<State>;

fn ROTRIGHT(x: u32, n: u32) -> u32 { return (x >> n) | (x << (32 - n)); }
fn CH(x: u32, y: u32, z: u32) -> u32 { return (x & y) ^ (~x & z); }
fn MAJ(x: u32, y: u32, z: u32) -> u32 { return (x & y) ^ (x & z) ^ (y & z); }
fn EP0(x: u32) -> u32 { return ROTRIGHT(x, 2) ^ ROTRIGHT(x, 13) ^ ROTRIGHT(x, 22); }
fn EP1(x: u32) -> u32 { return ROTRIGHT(x, 6) ^ ROTRIGHT(x, 11) ^ ROTRIGHT(x, 25); }
fn SIG0(x: u32) -> u32 { return ROTRIGHT(x, 7) ^ ROTRIGHT(x, 18) ^ (x >> 3); }
fn SIG1(x: u32) -> u32 { return ROTRIGHT(x, 17) ^ ROTRIGHT(x, 19) ^ (x >> 10); }

fn sha256_transform(ctx: ptr<function, State>, block: ptr<function, Block>) {
  var m: array<u32, 64>;
  var a: u32, b: u32, c: u32, d: u32, e: u32, f: u32, g: u32, h: u32;

  // 消息扩展
  for (var i: u32 = 0u; i < 16u; i++) {
    m[i] = (*block).data[i];
  }
  for (i = 16u; i < 64u; i++) {
    m[i] = SIG1(m[i-2]) + m[i-7] + SIG0(m[i-15]) + m[i-16];
  }

  // 初始化工作变量
  a = (*ctx).state[0];
  b = (*ctx).state[1];
  c = (*ctx).state[2];
  d = (*ctx).state[3];
  e = (*ctx).state[4];
  f = (*ctx).state[5];
  g = (*ctx).state[6];
  h = (*ctx).state[7];

  // 主循环
  for (i = 0u; i < 64u; i++) {
    let t1 = h + EP1(e) + CH(e, f, g) + K[i] + m[i];
    let t2 = EP0(a) + MAJ(a, b, c);
    h = g;
    g = f;
    f = e;
    e = d + t1;
    d = c;
    c = b;
    b = a;
    a = t1 + t2;
  }

  // 更新状态
  (*ctx).state[0] += a;
  (*ctx).state[1] += b;
  (*ctx).state[2] += c;
  (*ctx).state[3] += d;
  (*ctx).state[4] += e;
  (*ctx).state[5] += f;
  (*ctx).state[6] += g;
  (*ctx).state[7] += h;
}

@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
  let blockIndex = global_id.x;
  var ctx: State;

  // 初始化状态（链式或独立）
  if (blockIndex == 0u) {
    ctx.state = array<u32, 8>(
      0x6a09e667u, 0xbb67ae85u, 0x3c6ef372u, 0xa54ff53au,
      0x510e527fu, 0x9b05688cu, 0x1f83d9abu, 0x5be0cd19u
    );
  } else {
    ctx.state = chainStates[blockIndex - 1u].state;
  }

  // 处理当前块
  var currentBlock = inputBlocks[blockIndex];
  sha256_transform(&ctx, &currentBlock);

  // 保存结果
  var hash: array<u32, 32>;
  for (var i = 0u; i < 8u; i++) {
    let val = ctx.state[i];
    hash[i*4u + 0u] = (val >> 24u) & 0xFFu;
    hash[i*4u + 1u] = (val >> 16u) & 0xFFu;
    hash[i*4u + 2u] = (val >> 8u) & 0xFFu;
    hash[i*4u + 3u] = val & 0xFFu;
  }
  outputHashes[blockIndex] = hash;

  // 更新链式状态
  chainStates[blockIndex] = ctx;
}