package freechips.rocketchip.tilelink

// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.diplomacy.{AddressSet, TransferSizes}
import freechips.rocketchip.util.DescribedSRAM
import _root_.freechips.rocketchip.util.DescribedSRAM
import chisel3.util.random.GaloisLFSR



case class CustomBankAllocParams (
    cacheSize : BigInt,
    cacheWays : Int,
    cacheBanks : Int,
    cacheBlockSize : Int,
    addressBits : Int,
    evictionQueueDepth : Int,
)

case class BankAllocEntry(tagBits : Int, bankBits: Int) extends Bundle {
    val tag = UInt(tagBits.W)
    val bank = UInt(bankBits.W)
}

case class BankAllocEviction(tagBits : Int, bankBits: Int, wayBits: Int) extends Bundle {
  val tag = UInt(tagBits.W)
  val bank = UInt(bankBits.W)
  val way = UInt(wayBits.W)
}


case class CheckEvictionEntry(tagBits : Int, bankBits: Int, wayBits: Int) extends Bundle {
  val evict = BankAllocEviction(tagBits, bankBits, wayBits)
  val valid = Bool()
}



class CustomBankAlloc(params: CustomBankAllocParams)(implicit p: Parameters) extends LazyModule 
{

    val tagBits = params.addressBits-log2Ceil(params.cacheBlockSize)
    val bankBits = log2Ceil(params.cacheBanks)
    val wayBits = log2Ceil(params.cacheWays)
    val nSets = params.cacheSize/(params.cacheWays*params.cacheBlockSize)
    val bank_alloc_dir =  DescribedSRAM(
      name = "bankALlocDir",
      desc = "Bank Alloc Directory",
      size = nSets, // sets
      data = Vec(params.cacheWays*params.cacheBanks, BankAllocEntry(tagBits, bankBits))
    )

    /*
        If we split the bank allocation policy we need to do the following:
        1. Instantiate a BankBinder node that uses the old policy for allocation
        2. Probably modify the OS to allow sharing/coherence? 
        3. Arbitrate outgoing edges to allow both policies to forward requests
    */



    lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    

        require(isPow2(params.cacheBanks), "params.cacheBanks is not a power of 2") // otherwise we will require extra logic. nBanks should always be a power of 2
        val randomBankAlloc = RegInit(0.U(log2Ceil(params.cacheBanks).W))

        // we can use an LSFR for pseudo random way allocation
        val lsfr = Module(new GaloisLFSR(4, Set(4,3)))


        // i think we can leave return edges the same



      /*
        For incoming request:
        1. calculate set
        2. get way entries for that set
        3. compare address to tag entry for all way entries + eviction FIFO
        4. if entry is found in directory -> send request to that bank
        5. if entry is found in eviction fifo -> we can allocate it on a new bank and send to that bank
        6. if entry not found -> we need to allocate a new entry and then send to bank we allocate from
      */
      val incoming_tag = 0.U // get from incoming edges
      val incoming_set = 0.U // actually calculate
      val readWayEntries = bank_alloc_dir.read(incoming_set, true.B)

      val incoming_tag_matches = readWayEntries.map(_.tag === incoming_tag) 
      val incoming_tag_hit = incoming_tag_matches.reduce(_ || _) // will be high if any hit
      val incoming_hit_index = Mux1H(incoming_tag_matches, VecInit((0 until params.cacheWays*params.cacheBanks).map(_.U))) // get index of the hit
      val incoming_hit_entry = readWayEntries(incoming_hit_index)

      /*
        For eviction:
        1. We send incoming evictions to eviction FIFO
        2. We should be able to write ? per cycle
      */
      val incoming_eviction = BankAllocEviction(tagBits, bankBits, wayBits) // placeholder
      val eviction_fifo = Module(new Queue(new BankAllocEviction(tagBits, bankBits, wayBits), entries = params.evictionQueueDepth)) // 16-entry FIFO
      val eviction_check_buffer = Vec(params.evictionQueueDepth, Reg(new CheckEvictionEntry(tagBits, bankBits, wayBits))) // parallel buffer for checks
      val check_buffer_write_index = eviction_fifo.enq_ptr.value
      val incoming_eviction_buffer_check = eviction_check_buffer.map(entry => entry.evict.tag === incoming_tag && entry.valid) // compare incoming tag to all in eviction buffer
      val incoming_eviction_buffer_hit = incoming_eviction_buffer_check.reduce(_ || _) // check if any hit in eviction buffer
      val incoming_eviction_match_index = Mux1H(incoming_eviction_buffer_check, VecInit((0 until params.evictionQueueDepth).map(_.U)))
      
      


      /*
        To perform evictions we must perform a read-modify-write operation
        We can do this in two cycles

      */
      eviction_fifo.io.deq.ready := (writeState === sReadReady) // write must be currently idling


      val sReadReady :: sWrite :: Nil = Enum(4)
      val writeState = RegInit(sReadReady)
      val tmpBufferEvictReg = Vec(params.cacheWays*params.cacheBanks, Reg(BankAllocEntry(tagBits, bankBits)))
      val evictSet = RegInit(0.U(log2Ceil(nSets).W))
      when (eviction_fifo.io.deq.fire)
      {

          
          evictSet := eviction_fifo.io.deq.bits.tag // REPLACE WITH SET
          val evictBank = eviction_fifo.io.deq.bits.bank
          val evictWay = eviction_fifo.io.deq.bits.way
          tmpBufferEvictReg := bank_alloc_dir.read(evictSet, true.B)
          tmpBufferEvictReg(evictBank*evictWay) := 0.U // just zero it out
          writeState := sWrite
      }
      when (writeState === sWrite)
      {
          bank_alloc_dir.write(evictSet, tmpBufferEvictReg)
          writeState := sReadReady
      }


  }
}