package tk.monnef.mcmapper.test

import org.scalatest.{Matchers, FlatSpec}
import tk.monnef.mcmapper._
import tk.monnef.mcmapper.MethodMapping
import tk.monnef.mcmapper.ClassMapping
import java.io.File

class CompleteParsingTest extends FlatSpec with Matchers {

  import TestHelper._
  import McMapper._
  import MappingSide._

  val mcp811path = resPath + "mcp/811/"


  "MCP811" should "be present" taggedAs MCPTest in {
    if (!new File(mcp811path).exists()) fail()
  }

  "McMapper" should "properly parse MCP811" taggedAs MCPTest in {
    RawDataMerger.enableDebugDumpFile(new File("debugDumpFile.txt"))
    val r = McMapper.load(mcp811path)
    r.fields.size shouldBe 6012
    r.methods.size shouldBe 11209
    r.classes.size shouldBe 1572

    r.classes.find(_.obf.whole.equals("cl")).get shouldEqual ClassMapping("cl", "net/minecraft/nbt/NBTBase", BOTH)
    r.fields.find(_.obf.whole.equals("ul/aB")).get shouldEqual FieldMapping("ul/aB", "net/minecraft/entity/projectile/EntityFishHook/field_70215_au", "velocityX", "", CLIENT).constructWholeFull
    r.methods.find(_.obf.whole.equals("nn/T")).get shouldEqual MethodMapping("nn/T", "net/minecraft/entity/Entity/func_70089_S", "isEntityAlive", "()Z", "()Z", "Checks whether target entity is alive.", BOTH).constructWholeFull
    r.methods.find(a => a.obf.whole.equals("nn/a") && a.obfArgs.equals("(IIF)Lss;")).get shouldEqual MethodMapping("nn/a", "net/minecraft/entity/Entity/func_70054_a", "dropItemWithOffset", "(IIF)Lss;", "(IIF)Lnet/minecraft/entity/item/EntityItem;", "Drops an item stack with a specified y offset. Args: itemID, count, yOffset", BOTH).constructWholeFull
  }

  it should "be able to create and use search db" taggedAs MCPTest in {
    val r = McMapper.load(mcp811path)
    var db = MappingDatabaseSearchable(r)

    val c = db.searchClass("cl")
    c.size shouldBe 1
    c(0).full.whole shouldBe "net/minecraft/nbt/NBTBase"

    val f = db.searchField("ul/aB")
    f.size shouldBe 1
    f(0).full.short shouldBe "velocityX"

    var m = db.searchMethod("nn/T")
    m.size shouldBe 1
    m(0).full.short shouldBe "isEntityAlive"

    m = db.searchMethod("nn/a")
    m.size shouldBe 27
    assert(m.exists(_.fullArgs.equals("(Lnet/minecraft/world/Explosion;Lnet/minecraft/world/World;IIILnet/minecraft/block/Block;)F")))
    assert(m.forall(_.obf.whole.equals("nn/a")))
  }
}
