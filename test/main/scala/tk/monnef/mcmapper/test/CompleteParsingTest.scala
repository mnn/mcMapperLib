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

  "McMapper" should "properly parse MCP811" in {
    RawDataMerger.enableDebugDumpFile(new File("debugDumpFile.txt"))
    val r = McMapper.load(resPath + "mcp/811/")
    r.fields.size shouldBe 6012
    r.methods.size shouldBe 11209
    r.classes.size shouldBe 1572

    r.classes.find(_.obf.equals("cl")).get shouldEqual ClassMapping("cl", "net/minecraft/nbt/NBTBase", BOTH)
    r.fields.find(_.obf.equals("ul/aB")).get shouldEqual FieldMapping("ul/aB", "net/minecraft/entity/projectile/EntityFishHook/field_70215_au", "velocityX", "", CLIENT)
    r.methods.find(_.obf.equals("nn/T")).get shouldEqual MethodMapping("nn/T", "net/minecraft/entity/Entity/func_70089_S", "isEntityAlive", "()Z", "()Z", "Checks whether target entity is alive.", BOTH)
    r.methods.find(a => a.obf.equals("nn/a") && a.obfArgs.equals("(IIF)Lss;")).get shouldEqual MethodMapping("nn/a", "net/minecraft/entity/Entity/func_70054_a", "dropItemWithOffset", "(IIF)Lss;", "(IIF)Lnet/minecraft/entity/item/EntityItem;", "Drops an item stack with a specified y offset. Args: itemID, count, yOffset", BOTH)
  }
}
